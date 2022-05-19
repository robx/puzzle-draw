{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        name = "puzzld";

        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in pkgs.haskellPackages.developPackage {
            root = ./.;
            name = name;
            returnShellEnv = !(devTools == [ ]);
            source-overrides = { "SVGFonts" = "1.7.0.1"; };
            modifier = (t.flip t.pipe) [
              addBuildTools
              ((t.flip hl.addBuildTools) [ pkgs.imagemagick ])
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
              ((t.flip hl.overrideCabal) (drv: {
                postCheck = ''
                  patchShebangs tests/examples/gallery.sh
                  make compare DRAW=$(pwd)/dist/build/drawpuzzle/drawpuzzle
                '';
              }))
            ];
          };

      in {
        packages.puzzle-draw = project [ ];
        packages.puzzle-draw-web = pkgs.callPackage ./nix/web.nix {};

        defaultPackage = self.packages.${system}.puzzle-draw;

        devShell = project ([
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.cabal-install
        ]);
      });
}
