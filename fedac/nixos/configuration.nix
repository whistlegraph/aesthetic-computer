{ config, pkgs, lib, self ? null, gitHash ? "unknown", version ? "dev", ... }:

let
  ac-native = pkgs.callPackage ./packages/ac-native { inherit gitHash version; };
in
{
  imports = [
    ./modules/hardware.nix
    ./modules/kiosk.nix
    ./modules/wifi.nix
  ];

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "quiet"
    "loglevel=3"
    "vt.global_cursor_default=0"
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
    useDHCP = lib.mkDefault true;
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

  # Autologin — no greeter, no display manager
  services.getty.autologinUser = "ac";

  # System packages — only what ac-native needs at runtime
  environment.systemPackages = with pkgs; [
    ac-native
    cage
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
    description = "Mount USB config partition at /mnt";
    wantedBy = [ "multi-user.target" ];
    before = [ "ac-native-kiosk.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "mount-usb-config" ''
        # Scan for FAT32 partition with config.json
        for dev in /dev/sda1 /dev/sdb1 /dev/sdc1 /dev/sdd1; do
          [ -b "$dev" ] || continue
          mount -t vfat -o ro "$dev" /mnt 2>/dev/null || continue
          if [ -f /mnt/config.json ] || [ -f /mnt/EFI/BOOT/BOOTX64.EFI ]; then
            echo "Mounted config from $dev"
            exit 0
          fi
          umount /mnt
        done
        echo "No USB config partition found"
      '';
    };
  };

  # NixOS release
  system.stateVersion = "24.11";
}
