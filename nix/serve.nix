{ src, pkgs, system, puzzle-draw, puzzle-draw-web }:
derivation {
  inherit src system;
  name = "puzzle-draw-serve";
  builder = "${pkgs.bash}/bin/bash";
  args = [
    "-c"
    ''
      PATH=${pkgs.coreutils}/bin:$PATH
      mkdir -p $out/bin
      cp ${puzzle-draw}/bin/servepuzzle $out/bin/
      mkdir -p $out/static
      cp $src/static/* $out/static/
      cp -r $src/tests/examples $out/static/
      cp ${puzzle-draw-web}/web.min.js $out/static/web.js
      # post-cp comment because nix is insane: https://github.com/NixOS/nix/issues/2176
    ''
  ];
}
