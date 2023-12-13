{config, pkgs, ...}:
{
  _module.args.upkgs = import <nixos-unstable> {
    inherit (config.nixpkgs) config;
    localSystem = pkgs.stdenv.hostPlatform;
  };
}
