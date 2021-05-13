{
  description = "My Packages";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        {
          defaultPackage = import ./config.nix { inherit nixpkgs; inherit system; };
        }
      );
}