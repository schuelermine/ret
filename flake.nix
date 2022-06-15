{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default =
        nixpkgs.legacyPackages.${system}.haskellPackages.callPackage
        ./package.nix { };
    }) // {
      overlays.default = final: prev: {
        ret = self.packages.${final.system}.default;
      };
    };
}
