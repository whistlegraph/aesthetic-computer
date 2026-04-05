{
  description = "AC Native OS — NixOS kiosk for Aesthetic Computer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      lib = nixpkgs.lib;
      version = builtins.substring 0 8 (self.lastModifiedDate or "unknown");
      gitHash = self.shortRev or "dirty";
      nativeSrcPath = builtins.getEnv "AC_NIX_NATIVE_SRC";
      nativeSrc =
        if nativeSrcPath != "" then
          builtins.path {
            path = nativeSrcPath;
            name = "ac-native-source";
          }
        else
          throw "AC_NIX_NATIVE_SRC is required for fedac/nixos builds; run nix with --impure and point it at fedac/native.";
      specialArgs = { inherit self gitHash version nativeSrc; };
      runtimeModules = [ ./configuration.nix ];
      imageModules = runtimeModules ++ [ ./modules/image.nix ];
      evalConfig = import "${nixpkgs}/nixos/lib/eval-config.nix";
      makeDiskImage = import "${nixpkgs}/nixos/lib/make-disk-image.nix";
      runtimeSystem = lib.nixosSystem {
        inherit system;
        modules = runtimeModules;
        inherit specialArgs;
      };
      imageSystem = evalConfig {
        inherit system;
        modules = imageModules;
        inherit specialArgs;
      };
    in
    {
      # The ac-native binary as a standalone package
      packages.${system} = {
        ac-native = pkgs.callPackage ./packages/ac-native {
          inherit gitHash version nativeSrc;
        };

        # Bootable raw disk image with BIOS + UEFI bootloader install.
        # In nixpkgs make-disk-image, "hybrid" is GPT with an ESP plus
        # a bios_grub partition, not a hybrid MBR.
        usb-image = makeDiskImage {
          inherit pkgs lib;
          config = imageSystem.config;
          format = "raw";
          onlyNixStore = false;
          partitionTableType = "hybrid";
          installBootLoader = true;
          touchEFIVars = false;
          copyChannel = false;
          # The generic "boots anywhere" initrd is large, so give the ESP
          # enough room for the kernel, initrd, and GRUB assets.
          bootSize = "2048M";
          diskSize = "auto";
          additionalSpace = "2G";
          memSize = 4096;
        };

        default = self.packages.${system}.usb-image;
      };

      # Full NixOS system configuration
      nixosConfigurations.ac-native-os = runtimeSystem;
      nixosConfigurations.ac-native-image = imageSystem;
    };
}
