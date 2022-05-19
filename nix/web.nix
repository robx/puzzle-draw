{
  nixpkgs ? <nixpkgs>,
  config ? {},
}:
with (import nixpkgs config); let
  srcs = {
    "elm/browser" = {
      sha256 = "1apmvyax93nvmagwj00y16zx10kfv640cxpi64xgqbgy7d2wphy4";
      version = "1.0.0";
    };

    "elm/core" = {
      sha256 = "10kr86h4v5h4p0586q406a5wbl8xvr1jyrf6097zp2wb8sv21ylw";
      version = "1.0.0";
    };

    "elm/html" = {
      sha256 = "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k";
      version = "1.0.0";
    };

    "elm/http" = {
      sha256 = "1igmm89ialzrjib1j8xagkxalq1x2gj4l0hfxcd66mpwmvg7psl8";
      version = "1.0.0";
    };

    "elm/json" = {
      sha256 = "1g0hafkqf2q633r7ir9wxpb1lnlzskhpsyi0h5bkzj0gl072zfnb";
      version = "1.0.0";
    };

    "elm/time" = {
      sha256 = "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1";
      version = "1.0.0";
    };

    "elm/url" = {
      sha256 = "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4";
      version = "1.0.0";
    };

    "elm/virtual-dom" = {
      sha256 = "0hm8g92h7z39km325dlnhk8n00nlyjkqp3r3jppr37k2k13md6aq";
      version = "1.0.0";
    };
  };
in
  stdenv.mkDerivation {
    src = ./.;
    name = "puzzle-draw-web";

    buildInputs = [elmPackages.elm nodePackages.uglify-js];

    buildPhase = pkgs.elmPackages.fetchElmDeps {
      elmPackages = srcs;
      elmVersion = "0.19.1";
      registryDat = "./registry.dat";
    };

    installPhase = ''
      elm make src/Main.elm --output $out/web.js
      uglifyjs $out/web.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                | uglifyjs --mangle --output $out/web.min.js
    '';
  }
