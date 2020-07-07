{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskellPackages.ghcWithPackages (p: [ p.tensorflow ]);
in
pkgs.mkShell {
  buildInputs = [   pkgs.python37
                    pkgs.python37Packages.numpy
                    pkgs.python37Packages.regex
                    pkgs.python37Packages.requests
                    pkgs.python37Packages.tensorflowWithCuda
                    pkgs.python37Packages.tqdm
                    pkgs.python37Packages.toposort
                    pkgs.shake
                    hs ];
 shellHook = ''
    export CUDA_PATH=${pkgs.cudatoolkit}
  '';

}
