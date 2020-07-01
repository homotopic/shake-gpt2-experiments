{ sources ? import ./nix/sources.nix {}
, pkgs ? import sources.nixpkgs {} }:

let
  hsPkgs = import ./nix/default.nix;
in
  hsPkgs.shellFor {
    withHoogle = true;

    packages = ps: with ps; [mediawiki-shell];

    buildInputs = [
      pkgs.haskellPackages.wai-app-static
      pkgs.haskellPackages.shake
      pkgs.texlive.combined.scheme-basic
      pkgs.zlib.out
    ];

    LD_LIBRARY_PATH="${pkgs.zlib.out}/lib";
    exactDeps = true;
  }
