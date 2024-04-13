{
  inputs = {
    flaksie.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    
    nixpkgs-xr = {
      url = "github:nix-community/nixpkgs-xr";
      inputs.nixpkgs.follows = "flaksie";
    };
    
    #lemonake = {
    #  url = "github:passivelemon/lemonake";
    #  inputs.nixpkgs.follows = "flaksie";
    #};
  };
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
