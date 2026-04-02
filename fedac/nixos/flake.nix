{
  description = "AC Native OS — NixOS kiosk for Aesthetic Computer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-generators, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
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
    in
    {
      # The ac-native binary as a standalone package
      packages.${system} = {
        ac-native = pkgs.callPackage ./packages/ac-native {
          inherit gitHash version nativeSrc;
        };

        # Bootable ISO image (no KVM needed to build)
        usb-image = nixos-generators.nixosGenerate {
          inherit system;
          modules = [ ./configuration.nix ];
          format = "iso";
          specialArgs = { inherit self gitHash version nativeSrc; };
        };

        default = self.packages.${system}.usb-image;
      };

      # Full NixOS system configuration
      nixosConfigurations.ac-native-os = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./configuration.nix ];
        specialArgs = { inherit self gitHash version nativeSrc; };
      };
    };
}
