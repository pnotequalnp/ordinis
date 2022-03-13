{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    effectful-src = {
      url = "github:haskell-effectful/effectful";
      flake = false;
    };
    errata-src = {
      url = "github:1Computer1/errata";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, effectful-src, errata-src }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskell.packages.ghc921;
        effectful-core = hs.callCabal2nix "effectful-core" "${effectful-src}/effectful-core" { };
        effectful = hs.callCabal2nix "effectful" "${effectful-src}/effectful" { inherit effectful-core; };
        effectful-th = hs.callCabal2nix "effectful-th" "${effectful-src}/effectful-th" { inherit effectful; };
        errata = hs.callCabal2nix "errata" errata-src { hspec-golden = hs.hspec-golden_0_2_0_0; };
      in
      rec {
        packages.ordinis = hs.callCabal2nix "ordinis" ./. { inherit effectful errata; };
        defaultPackage = packages.ordinis;

        apps.ordinis = flake-utils.lib.mkApp { drv = packages.ordinis; };
        defaultApp = apps.ordinis;

        devShell = packages.ordinis.env.overrideAttrs
          (super: {
            buildInputs = with hs; super.buildInputs ++ [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          });
      });
}
