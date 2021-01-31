{ pins ? import ./haskell.nix {
    extraOverlays = [
      (self: super: {
        # Required by llvm-hs
        llvm-config = self.llvm_9;
      })
    ];
  }
, haskellNix ? pins.haskellNix
, pkgs ? pins.pkgs
, enableProfiling ? false
}: {
  inherit pkgs;
  project = pkgs.haskell-nix.project {
    # Although GHC 8.10.3 has been released and is available in nixpkgs master, haskell.nix doesn't yet support it.
    # See https://github.com/input-output-hk/haskell.nix/pull/980
    compiler-nix-name = "ghc8102";

    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "elemental";
      src = ./..;
    };

    modules = [
      {
        # haskell.nix currently ignores the profiling settings in cabal.project and cabal.project.local.
        # So we have to enable profiling in the Nix configuration if we want it.
        # See https://github.com/input-output-hk/haskell.nix/issues/887
        enableExecutableProfiling = enableProfiling;
        enableLibraryProfiling = enableProfiling;
      }
    ];
  };
}
