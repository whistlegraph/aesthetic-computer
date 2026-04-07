{ lib, ... }:

{
  # Use systemd-boot for fast EFI boot (GRUB adds ~2min on USB drives).
  boot.loader.systemd-boot.enable = lib.mkForce true;
  boot.loader.grub.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
  boot.loader.timeout = lib.mkForce 0;
  boot.growPartition = lib.mkDefault true;

  # Quiet boot — suppress kernel/systemd text, go straight to ac-native.
  # Suppress NixOS initrd "stage 1" / "stage 2" banners
  boot.initrd.verbose = lib.mkForce false;

  # Silent boot: redirect ALL output to tty2 (invisible).
  # tty1 stays black until cage takes over with ac-native.
  boot.kernelParams = lib.mkForce [
    "consoleblank=0"
    "console=tty0"
    "loglevel=4"
    "systemd.show_status=1"
    "mitigations=off"
  ];
  boot.consoleLogLevel = lib.mkForce 0;

  # Match nixpkgs raw-disk expectations so the installed image can boot on
  # real hardware and expand cleanly after flashing.
  fileSystems."/" = lib.mkForce {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    autoResize = true;
  };
  fileSystems."/boot" = lib.mkForce {
    device = "/dev/disk/by-label/ESP";
    fsType = "vfat";
  };
}
