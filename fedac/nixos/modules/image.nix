{ lib, ... }:

{
  # Raw disk image builds need a full installed bootloader, not the live ISO path.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
  boot.loader.grub = {
    enable = true;
    devices = [ "/dev/vda" ];
    efiSupport = true;
    efiInstallAsRemovable = true;
    configurationLimit = 1;
  };
  boot.loader.timeout = lib.mkForce 0;
  boot.loader.grub.timeoutStyle = lib.mkForce "hidden";
  boot.growPartition = lib.mkDefault true;

  # Make early boot text visible on the real display for debug + fallback boot paths.
  boot.kernelParams = lib.mkAfter [ "console=tty0" ];

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
