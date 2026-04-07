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
  ac-native-start = pkgs.writeShellScript "ac-native-start" ''
    set -u

    echo "[ac-native-start] waiting for DRM device..."

    # Wait for any /dev/dri/card* (may be card0 or card1 depending on hardware)
    for i in $(seq 1 60); do
      ls /dev/dri/card* >/dev/null 2>&1 && break
      sleep 0.2
    done

    if ! ls /dev/dri/card* >/dev/null 2>&1; then
      echo "[ac-native-start] ERROR: no DRM card found after 12s"
      ls -la /dev/dri/ 2>/dev/null || echo "  /dev/dri/ does not exist"
      exit 1
    fi

    echo "[ac-native-start] DRM ready: $(ls /dev/dri/)"

    # Hardware fingerprint — dump to /mnt/logs for initrd optimization
    if [ -d /mnt/logs ]; then
      HW="/mnt/logs/hardware-fingerprint.txt"
      {
        echo "=== AC Native Hardware Fingerprint ==="
        echo "date=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""
        echo "=== CPU ==="
        grep -m1 "model name" /proc/cpuinfo 2>/dev/null
        echo "cores=$(nproc 2>/dev/null)"
        echo ""
        echo "=== Memory ==="
        head -1 /proc/meminfo 2>/dev/null
        echo ""
        echo "=== PCI Devices ==="
        ${pkgs.pciutils}/bin/lspci -nn 2>/dev/null
        echo ""
        echo "=== Loaded Kernel Modules ==="
        lsmod 2>/dev/null | sort
        echo ""
        echo "=== Block Devices ==="
        lsblk -o NAME,SIZE,TYPE,FSTYPE,LABEL,MODEL 2>/dev/null
        echo ""
        echo "=== GPU ==="
        ls -la /dev/dri/ 2>/dev/null
        cat /sys/class/drm/card*/device/vendor /sys/class/drm/card*/device/device 2>/dev/null
        echo ""
        echo "=== Input Devices ==="
        cat /proc/bus/input/devices 2>/dev/null
        echo ""
        echo "=== Network Interfaces ==="
        ip link show 2>/dev/null
        echo ""
        echo "=== Sound Cards ==="
        cat /proc/asound/cards 2>/dev/null
        echo ""
        echo "=== USB Devices ==="
        ${pkgs.usbutils}/bin/lsusb 2>/dev/null
        echo ""
        echo "=== Firmware Files Loaded ==="
        dmesg 2>/dev/null | grep -i "firmware\|ucode\|microcode" | head -20
        echo ""
        echo "=== Initrd Modules (loaded at boot) ==="
        dmesg 2>/dev/null | grep -i "module\|driver" | head -30
      } > "$HW" 2>/dev/null || true
      echo "[ac-native-start] hardware fingerprint saved to $HW"
    fi

    ${write-breadcrumb} ac-native-starting \
      "binary=${ac-native}/bin/ac-native" \
      "piece=${ac-native}/share/ac-native/piece.mjs" \
      "mode=drm-direct"

    # Switch to tty1 and run ac-native in DRM-direct mode.
    chvt 1

    echo "[ac-native-start] launching ac-native DRM-direct"
    exec "${ac-native}/bin/ac-native" \
      "${ac-native}/share/ac-native/piece.mjs"
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
  # DRM-direct kiosk: ac-native owns the display and input hardware.
  # No cage compositor, no seatd, no Wayland — matching the bare-metal build.
  systemd.services.ac-native-kiosk = {
    description = "AC Native OS kiosk (DRM direct)";
    conflicts = [ "getty@tty1.service" ];
    after = [ "getty@tty1.service" "mount-usb-config.service" ];
    wants = [ "mount-usb-config.service" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [
      coreutils gnugrep gnused gawk findutils
      which psmisc
      systemd util-linux
      iproute2 kbd pciutils usbutils
      wpa_supplicant iw dhcpcd curl
      dosfstools efibootmgr parted
      ac-native
    ];

    environment = {
      HOME = "/tmp/ac-home";
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
      ALSA_CONFIG_PATH = "${pkgs.alsa-lib}/share/alsa/alsa.conf";
    };

    serviceConfig = {
      # Run as root for DRM master + evdev + WiFi + ALSA access.
      Type = "simple";
      Restart = "on-failure";
      RestartSec = 2;
      TTYPath = "/dev/tty1";
      TTYReset = true;
      TTYVHangup = true;
      TTYVTDisallocate = true;
      # ac-native reads evdev directly, not stdin. Using "tty" here
      # causes systemd to grab tty1 input, blocking keyboard events.
      StandardInput = "null";
      StandardOutput = "journal+console";
      StandardError = "journal+console";

      ExecStart = "${ac-native-start}";

      # Exit code: 0 = shutdown, 2 = reboot
      SuccessExitStatus = "0 2";
      ExecStopPost = "+${ac-native-stop}";
    };
  };

  systemd.tmpfiles.rules = [
    "d /mnt 0755 root root -"
    "d /run/user/0 0700 root root -"
    "d /tmp/ac-home 0700 root root -"
    "L+ /piece.mjs - - - - ${ac-native}/share/ac-native/piece.mjs"
    "L+ /pieces - - - - ${ac-native}/share/ac-native/pieces"
    "L+ /jslib - - - - ${ac-native}/share/ac-native/jslib"
  ];
}
