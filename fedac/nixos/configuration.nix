{ config, pkgs, lib, self ? null, gitHash ? "unknown", version ? "dev", nativeSrc, kidlispSrc ? null, ... }:

let
  ac-native = pkgs.callPackage ./packages/ac-native { inherit gitHash version nativeSrc kidlispSrc; };
in
{
  imports = [
    ./modules/hardware.nix
    ./modules/kiosk.nix
    ./modules/wifi.nix
  ];

  system.nixos.distroName = "AC Native";

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.timeout = lib.mkDefault 3;
  boot.loader.grub.timeoutStyle = lib.mkDefault "menu";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "consoleblank=0"
  ];
  boot.consoleLogLevel = 0;

  # Minimal system — no desktop, no SSH, no docs
  documentation.enable = false;
  services.xserver.enable = false;
  services.openssh.enable = false;
  security.polkit.enable = true;

  # logind: ignore all hardware buttons (ac-native handles power via evdev).
  # Keep logind running — systemctl needs dbus which needs logind.
  services.logind.lidSwitch = "ignore";
  services.logind.powerKey = "ignore";
  services.logind.suspendKey = "ignore";
  services.logind.hibernateKey = "ignore";

  # Networking (WiFi managed by ac-native, not NetworkManager)
  networking = {
    hostName = "ac-native";
    # ac-native invokes dhcpcd itself after selecting a WiFi network.
    # Leave the system-level DHCP service disabled so boot does not block the kiosk.
    useDHCP = false;
    networkmanager.enable = false;
  };

  # Timezone
  time.timeZone = "America/Los_Angeles";

  # Kiosk user
  users.users.ac = {
    isNormalUser = true;
    extraGroups = [ "video" "audio" "input" "seat" "tty" ];
    home = "/tmp/ac-home";
  };

  # No getty — kiosk service takes over tty1 directly
  services.getty.autologinUser = lib.mkForce null;

  # System packages — only what ac-native needs at runtime
  environment.systemPackages = with pkgs; [
    ac-native
    wpa_supplicant
    iw
    dhcpcd
    curl
    util-linux    # sfdisk, blockdev for NVMe install
    dosfstools    # mkfs.vfat
    efibootmgr
    parted
    ffmpeg-full   # SRT streaming + VAAPI hardware encode
    libva-utils   # vainfo for debugging GPU encode
  ];

  # zram swap
  zramSwap = {
    enable = true;
    memoryPercent = 50;
  };

  # tmpfs for /tmp
  boot.tmp.useTmpfs = true;

  # Mount USB config partition at /mnt
  # ac-native reads /mnt/config.json and /mnt/wifi_creds.json
  systemd.services.mount-usb-config = {
    description = "Mount AC Native USB data partition at /mnt";
    wantedBy = [ "multi-user.target" ];
    before = [ "ac-native-kiosk.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      StandardOutput = "journal";
      StandardError = "journal";
      ExecStart = pkgs.writeShellScript "mount-usb-config" ''
        set -u

        write_breadcrumb() {
          local tag="$1"
          local stamp
          stamp="$(${pkgs.coreutils}/bin/date -u +%Y%m%dT%H%M%SZ)"
          {
            echo "tag=''${tag}"
            echo "stamp=''${stamp}"
            echo "host=ac-native"
            echo "version=${gitHash}-${version}"
          } > "/mnt/logs/''${tag}-''${stamp}.txt"
        }

        mkdir -p /mnt
        ${pkgs.systemd}/bin/udevadm settle --timeout=10 || true

        for _ in $(seq 1 40); do
          dev="$(${pkgs.util-linux}/bin/blkid -L ACDATA 2>/dev/null || true)"
          if [ -b "$dev" ] &&
             mount -t vfat \
               -o rw,uid=1000,gid=100,umask=0077,shortname=mixed,utf8=1 \
               "$dev" /mnt 2>/dev/null; then
            mkdir -p /mnt/logs
            write_breadcrumb "boot-mounted"
            echo "Mounted AC Native data from $dev"
            exit 0
          fi
          sleep 0.25
        done

        chown ac:users /mnt 2>/dev/null || true
        chmod 0700 /mnt 2>/dev/null || true
        mkdir -p /mnt/logs
        write_breadcrumb "boot-mounted-temporary"
        echo "No ACDATA partition found; /mnt is temporary"
        ${pkgs.util-linux}/bin/lsblk -o NAME,SIZE,TYPE,FSTYPE,LABEL,MOUNTPOINTS || true
        ${pkgs.util-linux}/bin/blkid || true
      '';
    };
  };

  # ALSA tee: "default" sends audio to both physical speakers and the loopback
  # device. The stream service captures from the loopback's read side.
  # snd_aloop is always loaded (see hardware.nix kernelModules).
  environment.etc."asound.conf".text = ''
    # AC Native audio tee — duplicate playback to speakers + ALSA loopback.
    # Loopback capture side (hw:Loopback,1,0) is read by the stream service.

    pcm.!default {
      type plug
      slave.pcm "ac_tee"
    }

    pcm.ac_tee {
      type route
      slave.pcm {
        type multi
        slaves {
          a { pcm "plughw:0,0" channels 2 }
          b { pcm "plughw:Loopback,0,0" channels 2 }
        }
        bindings {
          0 { slave a channel 0 }
          1 { slave a channel 1 }
          2 { slave b channel 0 }
          3 { slave b channel 1 }
        }
      }
      ttable.0.0 1
      ttable.1.1 1
      ttable.0.2 1
      ttable.1.3 1
    }

    ctl.!default {
      type hw
      card 0
    }
  '';

  # SRT streamer: pushes notepat framebuffer over WiFi to OBS on Windows,
  # which then forwards to TikTok Live.
  #
  # Opt-in: only starts if /mnt/stream.conf exists on the USB config partition.
  # Create /mnt/stream.conf with:
  #   HOST=192.168.1.100    # Windows host IP on LAN
  #   PORT=9000             # SRT port (OBS Media Source listens here)
  #   BITRATE=6M            # encode bitrate
  #   FRAMERATE=60          # capture fps
  #   SRT_LATENCY=40        # ms — lower = less latency, more fragile on bad WiFi
  #   WIDTH=1280            # output width (scale from native)
  #   HEIGHT=720            # output height
  systemd.services.ac-native-stream = {
    description = "AC Native SRT streamer (to OBS → TikTok)";
    after = [ "ac-native-kiosk.service" "mount-usb-config.service" ];
    wants = [ "ac-native-kiosk.service" ];
    wantedBy = [ "multi-user.target" ];

    unitConfig = {
      ConditionPathExists = "/mnt/stream.conf";
    };

    environment = {
      LIBVA_DRIVER_NAME = "iHD";
      ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
      ALSA_CONFIG_PATH = "${pkgs.alsa-lib}/share/alsa/alsa.conf";
    };

    serviceConfig = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = 3;
      EnvironmentFile = "/mnt/stream.conf";

      ExecStartPre = pkgs.writeShellScript "ac-native-stream-wait" ''
        # Give the kiosk a moment to grab DRM master and start rendering
        sleep 2

        for _ in $(seq 1 40); do
          ls /dev/dri/card* >/dev/null 2>&1 && break
          sleep 0.25
        done

        if ! ls /dev/dri/card* >/dev/null 2>&1; then
          echo "[stream] no DRM card found"
          exit 1
        fi
      '';

      ExecStart = pkgs.writeShellScript "ac-native-stream" ''
        set -u

        HOST="''${HOST:?HOST not set in /mnt/stream.conf}"
        PORT="''${PORT:-9000}"
        BITRATE="''${BITRATE:-6M}"
        FRAMERATE="''${FRAMERATE:-60}"
        SRT_LATENCY="''${SRT_LATENCY:-40}"
        WIDTH="''${WIDTH:-1280}"
        HEIGHT="''${HEIGHT:-720}"

        CARD="$(ls /dev/dri/card* 2>/dev/null | head -n1)"
        [ -n "$CARD" ] || { echo "[stream] no DRM card"; exit 1; }

        URL="srt://''${HOST}:''${PORT}?mode=caller&latency=''${SRT_LATENCY}&pkt_size=1316&tlpktdrop=1"
        echo "[stream] capturing $CARD at ''${FRAMERATE}fps → $URL"
        echo "[stream] encode: h264_vaapi ''${WIDTH}x''${HEIGHT} @ ''${BITRATE}"

        AUDIO_BITRATE="''${AUDIO_BITRATE:-128k}"

        exec ${pkgs.ffmpeg-full}/bin/ffmpeg \
          -hide_banner -loglevel warning \
          -threads 2 \
          -fflags nobuffer -flags low_delay \
          -device "$CARD" \
          -framerate "$FRAMERATE" \
          -f kmsgrab -i - \
          -f alsa -thread_queue_size 512 -channels 2 \
            -sample_rate 48000 -audio_buffer_size 10000 \
            -i hw:Loopback,1,0 \
          -vf "hwmap=derive_device=vaapi,scale_vaapi=w=''${WIDTH}:h=''${HEIGHT}:format=nv12" \
          -c:v h264_vaapi \
            -b:v "$BITRATE" -maxrate "$BITRATE" -bufsize "$BITRATE" \
            -g "$FRAMERATE" -bf 0 \
            -quality 1 \
            -compression_level 1 \
            -sei 0 \
          -c:a aac -b:a "$AUDIO_BITRATE" \
          -f mpegts "$URL"
      '';
    };
  };

  # NixOS release
  system.stateVersion = "24.11";
}
