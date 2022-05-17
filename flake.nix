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
            root = pkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" "LICENSE" ];
            name = name;
            returnShellEnv = !(devTools == [ ]);

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in {
        packages.puzzle-draw = project [ ];

        defaultPackage = self.packages.${system}.puzzle-draw;

        devShell = project (with pkgs.haskellPackages; [
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.cabal-install
        ]);
      });
}
