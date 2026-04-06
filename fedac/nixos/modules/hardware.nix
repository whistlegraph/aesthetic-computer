{ config, pkgs, lib, ... }:

{
  # Broad hardware support — load ALL firmware + WiFi regulatory DB
  hardware.firmware = [ pkgs.linux-firmware pkgs.wireless-regdb ];

  # GPU: Intel + AMD + Nvidia (nouveau)
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver  # iHD for Broadwell+
      intel-vaapi-driver   # i965 for older Intel (Sandy Bridge, etc.)
      mesa
    ];
  };

  # Audio: ALSA only (ac-native talks to ALSA directly)
  security.rtkit.enable = true;
  services.pipewire.enable = false;

  # Slim initrd — only modules needed for Intel laptops + USB boot.
  # Default NixOS initrd is 1.4GB (every driver); this cuts to ~50-80MB.
  boot.initrd.includeDefaultModules = false;
  boot.initrd.availableKernelModules = [
    # Storage (USB boot + internal NVMe)
    "ahci" "nvme" "sd_mod" "usb_storage" "uas"
    "xhci_pci" "ehci_pci" "ohci_pci"
    # Input
    "usbhid" "hid_generic" "hid_multitouch"
    # GPU (loaded early for KMS)
    "i915" "amdgpu" "nouveau"
    # Filesystems
    "ext4" "vfat" "nls_cp437" "nls_iso8859_1"
  ];

  boot.kernelModules = [
    # WiFi
    "iwlwifi" "iwlmvm"
    # Audio
    "snd_hda_intel" "snd_hda_codec_realtek" "snd_hda_codec_hdmi"
    "snd_usb_audio"
    # Bluetooth
    "btusb" "btintel"
  ];

  # Performance
  powerManagement.cpuFreqGovernor = "performance";

  # Console font (small, clean)
  console.font = "ter-v16n";
  console.packages = [ pkgs.terminus_font ];
}
