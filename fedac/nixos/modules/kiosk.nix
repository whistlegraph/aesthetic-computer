{ config, pkgs, lib, gitHash ? "unknown", version ? "dev", nativeSrc, ... }:

let
  ac-native = pkgs.callPackage ../packages/ac-native { inherit gitHash version nativeSrc; };
  ac-native-client = pkgs.writeShellScript "ac-native-cage-client" ''
    set -u

    rm -f /tmp/ac-native-cage.log
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
      coreutils util-linux
      wpa_supplicant iw dhcpcd curl
      dosfstools efibootmgr parted
      ac-native
    ];

    environment = {
      XDG_RUNTIME_DIR = "/run/user/1000";
      HOME = "/tmp/ac-home";
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
    };

    serviceConfig = {
      User = "ac";
      Group = "users";
      SupplementaryGroups = [ "video" "audio" "input" "seat" ];
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
      ExecStopPost = pkgs.writeShellScript "ac-native-stop" ''
        STATUS=$EXIT_STATUS
        if [ "$STATUS" = "0" ]; then
          systemctl poweroff
        elif [ "$STATUS" = "2" ]; then
          systemctl reboot
        fi
      '';

      # Security
      ProtectSystem = "strict";
      ReadWritePaths = [ "/tmp" "/mnt" "/run" ];
      PrivateTmp = false;  # ac-native uses /tmp for scratch
    };
  };

  # Ensure XDG_RUNTIME_DIR exists for the ac user
  systemd.tmpfiles.rules = [
    "d /mnt 0755 root root -"
    "d /run/user/1000 0700 ac users -"
    "d /tmp/ac-home 0700 ac users -"
    "L+ /piece.mjs - - - - ${ac-native}/share/ac-native/piece.mjs"
    "L+ /pieces - - - - ${ac-native}/share/ac-native/pieces"
  ];
}
