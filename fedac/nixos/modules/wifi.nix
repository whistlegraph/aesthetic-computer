{ config, pkgs, lib, ... }:

{
  # Do NOT enable networking.wireless (NixOS-managed wpa_supplicant).
  # ac-native's wifi.c manages wpa_supplicant directly via system() calls.
  # We just need the binaries available in PATH.
  networking.wireless.enable = false;

  # Ensure rfkill doesn't block WiFi
  boot.kernelModules = [ "rfkill" ];

  # udev rule to unblock WiFi on boot
  services.udev.extraRules = ''
    # Unblock WiFi interfaces on hotplug
    ACTION=="add", SUBSYSTEM=="rfkill", ATTR{type}=="wlan", RUN+="${pkgs.util-linux}/bin/rfkill unblock wifi"
  '';
}
