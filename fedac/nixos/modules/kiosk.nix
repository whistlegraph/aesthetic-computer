{ config, pkgs, lib, gitHash ? "unknown", version ? "dev", nativeSrc, kidlispSrc ? null, ... }:

let
  ac-native = pkgs.callPackage ../packages/ac-native { inherit gitHash version nativeSrc kidlispSrc; };
  write-breadcrumb = pkgs.writeShellScript "ac-native-write-breadcrumb" ''
    set -u

    [ $# -ge 1 ] || exit 0
    [ -d /mnt/logs ] || exit 0

    tag="$1"
    shift || true
    stamp="$(${pkgs.coreutils}/bin/date -u +%Y%m%dT%H%M%SZ)"
    out="/mnt/logs/''${tag}-''${stamp}.txt"
    {
      echo "tag=''${tag}"
      echo "stamp=''${stamp}"
      echo "version=${gitHash}-${version}"
      for entry in "$@"; do
        echo "$entry"
      done
    } > "$out" 2>/dev/null || true
    sync || true
  '';
  ac-native-client = pkgs.writeShellScript "ac-native-cage-client" ''
    set -u

    rm -f /tmp/ac-native-cage.log
    ${write-breadcrumb} ac-native-starting "binary=${ac-native}/bin/ac-native" "piece=${ac-native}/share/ac-native/piece.mjs"
    printf '[ac-native-cage-client] starting %s %s\n' \
      "${ac-native}/bin/ac-native" \
      "${ac-native}/share/ac-native/piece.mjs" >&2

    status=0
    "${ac-native}/bin/ac-native" \
      "${ac-native}/share/ac-native/piece.mjs" \
      >/tmp/ac-native-cage.log 2>&1 || status=$?

    printf '[ac-native-cage-client] ac-native exited status=%s\n' "$status" >&2
    if [ -s /tmp/ac-native-cage.log ]; then
      ${pkgs.gnused}/bin/sed 's/^/[ac-native-cage-log] /' \
        /tmp/ac-native-cage.log >&2 || true
    else
      printf '[ac-native-cage-client] no /tmp/ac-native-cage.log output\n' >&2
    fi

    exit "$status"
  '';
  ac-native-kiosk = pkgs.writeShellScript "ac-native-kiosk" ''
    set -u

    rm -f /tmp/cage-stderr.log
    ${write-breadcrumb} kiosk-launching "tty=/dev/tty1" "display=cage"
    printf '[ac-native-kiosk] launching cage on tty1\n' >&2

    status=0
    ${pkgs.cage}/bin/cage -s -- ${ac-native-client} \
      2>/tmp/cage-stderr.log || status=$?

    printf '[ac-native-kiosk] cage exited status=%s\n' "$status" >&2
    if [ -s /tmp/cage-stderr.log ]; then
      ${pkgs.gnused}/bin/sed 's/^/[cage-stderr] /' /tmp/cage-stderr.log >&2 || true
    else
      printf '[ac-native-kiosk] no /tmp/cage-stderr.log output\n' >&2
    fi

    exit "$status"
  '';
  ac-native-stop = pkgs.writeShellScript "ac-native-stop" ''
    set -u

    STATUS=''${EXIT_STATUS:-}
    RESULT=''${SERVICE_RESULT:-unknown}

    if mountpoint -q /mnt; then
      mkdir -p /mnt/logs
      stamp="$(date -u +%Y%m%dT%H%M%SZ)-$$"
      journal="/mnt/logs/ac-native-kiosk-''${RESULT}-''${STATUS:-na}-''${stamp}.journal.log"

      journalctl -b --no-pager \
        -u mount-usb-config.service \
        -u ac-native-kiosk.service \
        >"$journal" 2>&1 || true

      if [ -f /tmp/ac-native-cage.log ]; then
        cp /tmp/ac-native-cage.log "/mnt/logs/ac-native-cage-''${stamp}.log" || true
      fi
      if [ -f /tmp/cage-stderr.log ]; then
        cp /tmp/cage-stderr.log "/mnt/logs/cage-stderr-''${stamp}.log" || true
      fi
      sync || true
    fi

    if [ "$STATUS" = "0" ]; then
      systemctl poweroff
    elif [ "$STATUS" = "2" ]; then
      systemctl reboot
    fi
  '';
in
{
  # seatd for unprivileged GPU/input access
  services.seatd.enable = true;

  # Kiosk service: cage compositor running ac-native
  systemd.services.ac-native-kiosk = {
    description = "AC Native OS kiosk";
    conflicts = [ "getty@tty1.service" ];
    after = [ "getty@tty1.service" "mount-usb-config.service" "seatd.service" ];
    wants = [ "mount-usb-config.service" "seatd.service" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [
      coreutils gnugrep gnused gawk findutils
      which psmisc       # killall (psmisc), which
      systemd util-linux
      wpa_supplicant iw dhcpcd curl
      dosfstools efibootmgr parted
      ac-native
    ];

    environment = {
      XDG_RUNTIME_DIR = "/run/user/0";
      HOME = "/tmp/ac-home";
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
      ALSA_CONFIG_PATH = "${pkgs.alsa-lib}/share/alsa/alsa.conf";
      # Hide the Wayland compositor cursor — ac-native renders its own.
      WLR_NO_HARDWARE_CURSORS = "1";
      XCURSOR_SIZE = "1";
    };

    serviceConfig = {
      # Run as root — ac-native needs full hardware access (WiFi, ALSA,
      # DRM) matching the old bare-metal build where it ran as PID 1.
      # Security hardening can be layered on once all features work.
      Type = "simple";
      Restart = "on-failure";
      RestartSec = 2;
      TTYPath = "/dev/tty1";
      TTYReset = true;
      TTYVHangup = true;
      TTYVTDisallocate = true;
      StandardInput = "tty";
      StandardOutput = "journal+console";
      StandardError = "journal+console";

      # cage -s for single-app mode, wrapped so child logs land in journal.
      ExecStart = "${ac-native-kiosk}";

      # Exit code handling:
      #   0 = shutdown, 2 = reboot (matching current ac-native convention)
      SuccessExitStatus = "0 2";
      ExecStopPost = "+${ac-native-stop}";
    };
  };

  # Ensure XDG_RUNTIME_DIR exists for the ac user
  systemd.tmpfiles.rules = [
    "d /mnt 0755 root root -"
    "d /run/user/0 0700 root root -"
    "d /tmp/ac-home 0700 root root -"
    "L+ /piece.mjs - - - - ${ac-native}/share/ac-native/piece.mjs"
    "L+ /pieces - - - - ${ac-native}/share/ac-native/pieces"
    "L+ /jslib - - - - ${ac-native}/share/ac-native/jslib"
  ];
}
