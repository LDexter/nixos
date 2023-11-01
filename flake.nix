{
  inputs.flaksie.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  outputs = inputs @ {flaksie, ...}: {

    nixosConfigurations = {

      "NixPad" = flaksie.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./NixPad/configuration.nix
        ];
      };

      "NixTower" = flaksie.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./NixTower/configuration.nix
        ];
      };

    };
  };
}
