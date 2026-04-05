{ lib, ... }:

{
  # Raw disk image builds need a full installed bootloader, not the live ISO path.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
  boot.loader.grub = {
    enable = true;
    device = "/dev/vda";
    efiSupport = true;
    efiInstallAsRemovable = true;
    configurationLimit = 1;
  };
  boot.loader.timeout = lib.mkForce 0;
  boot.loader.grub.timeoutStyle = lib.mkForce "hidden";

  # Make early boot text visible on the real display for debug + fallback boot paths.
  boot.kernelParams = lib.mkAfter [ "console=tty0" ];

  # Let make-disk-image perform a full NixOS install onto a raw disk image.
  fileSystems."/" = lib.mkForce {
    device = "/dev/vda";
    fsType = "ext4";
    autoFormat = true;
  };
  virtualisation.useBootLoader = lib.mkForce true;
}
