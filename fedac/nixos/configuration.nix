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

  # NixOS release
  system.stateVersion = "24.11";
}
