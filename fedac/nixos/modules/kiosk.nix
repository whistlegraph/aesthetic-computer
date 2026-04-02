{ config, pkgs, lib, gitHash ? "unknown", version ? "dev", nativeSrc, ... }:

let
  ac-native = pkgs.callPackage ../packages/ac-native { inherit gitHash version nativeSrc; };
in
{
  # seatd for unprivileged GPU/input access
  services.seatd.enable = true;

  # Kiosk service: cage compositor running ac-native
  systemd.services.ac-native-kiosk = {
    description = "AC Native OS kiosk";
    after = [ "multi-user.target" "mount-usb-config.service" "seatd.service" ];
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

      # cage -s for single-app mode, -- separates cage args from app args
      ExecStart = "${pkgs.cage}/bin/cage -s -- ${ac-native}/bin/ac-native ${ac-native}/share/ac-native/piece.mjs";

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
    "d /run/user/1000 0700 ac users -"
    "d /tmp/ac-home 0700 ac users -"
  ];
}
