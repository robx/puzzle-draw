{ ghc }:
with (import <nixpkgs> {});

let
  native_libs = [
    pkgconfig
    cairo
    pango
  ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
    Foundation
  ]);

in haskell.lib.buildStackProject {
  inherit ghc;
  buildInputs = native_libs;
  name = "puzzleDrawBuildEnv";
  src = null;
}
