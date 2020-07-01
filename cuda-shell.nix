{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
   name = "cuda-env-shell";
   buildInputs = with pkgs;
                  [ python37
                    python37Packages.numpy
                    python37Packages.regex
                    python37Packages.requests
                    python37Packages.tensorflowWithCuda
                    python37Packages.tqdm
                    python37Packages.toposort
                    shake
                    haskellPackages.pandoc
                    haskellPackages.shake-plus];
   shellHook = ''
      export CUDA_PATH=${pkgs.cudatoolkit}
      # export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.ncurses5}/lib
		  export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
		  export EXTRA_CCFLAGS="-I/usr/include"
   '';          
}
