# default.nix
{ compilerVersion ? "ghc865", pkgsLink ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/1e90c46c2d98f9391df79954a74d14f263cad729.tar.gz)}:
let
  # Packages
  config = { allowBroken = true;
             allowUnfree = true;
           };
  pkgs = import pkgsLink { inherit config; };

  # Ensure working fonts with a fontconfig
  fontsConf = pkgs.makeFontsConf {
    fontDirectories = [];
  };

  # Haskell compilier
  compiler = pkgs.haskell.packages."${compilerVersion}";

  # BirchBeer package
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = {
      elbow = builtins.fetchTarball https://github.com/GregorySchwartz/elbow/archive/03fff043c88b8c3de83b08f1638963f46e604c90.tar.gz;
      sparse-linear-algebra = builtins.fetchTarball https://github.com/ocramz/sparse-linear-algebra/archive/dbad792f6c6a04e4de23806b676cb3e76d36a65b.tar.gz;
      spectral-clustering = builtins.fetchTarball https://github.com/GregorySchwartz/spectral-clustering/archive/8d735caeb26266beda299d7886b5586dc7d7e7b1.tar.gz;
    };
    overrides = self: super: (with pkgs.haskell.lib; with pkgs.haskellPackages; {
      BiobaseNewick = doJailbreak super.BiobaseNewick;
      clustering = dontBenchmark (dontCheck super.clustering);
      diagrams-graphviz = doJailbreak super.diagrams-graphviz;
      fgl = doJailbreak super.fgl;
      hmatrix-svdlibc = dontCheck (doJailbreak super.hmatrix-svdlibc);
      pipes-text = doJailbreak super.pipes-text;
      streaming-cassava = doJailbreak super.streaming-cassava;
      sparse-linear-algebra = dontCheck super.sparse-linear-algebra;
      typed-spreadsheet = doJailbreak super.typed-spreadsheet;
    });
  };
  buildInputs = [ pkgs.zlib
                  pkgs.zlib.dev
                  pkgs.zlib.out
                  pkgs.blas
                  pkgs.liblapack
                  pkgs.gfortran.cc.lib
                  pkgs.cairo
                  pkgs.stdenv
                  pkgs.gmp
                  pkgs.gsl
                  pkgs.gtk2
                  pkgs.pango
                  pkgs.graphviz
                  pkgs.pkg-config
                  pkgs.fontconfig
                  pkgs.makeWrapper
                  pkgs.glibcLocales
                  pkgs.ghcid
                  pkgs.cabal-install
               ];
in (pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs;
  nativeBuildInputs = attrs.nativeBuildInputs ++ buildInputs;
  postInstall = ''
            mkdir -p $out/paths

            wrapProgram $out/bin/birch-beer \
              --prefix 'PATH' ':' "${pkgs.graphviz}/bin/" \
              --set 'LANG' 'en_US.UTF-8' \
              --set 'LOCALE_ARCHIVE' "${pkgs.glibcLocales}/lib/locale/locale-archive" \
              --set 'FONTCONFIG_FILE' "${fontsConf}"
            '';
}))
