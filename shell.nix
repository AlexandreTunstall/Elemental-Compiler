{ defaultOpts ? {
    enableProfiling = true;
  }
}:
let
  hsPkgs = import ./nix/default.nix defaultOpts;
in
  hsPkgs.project.shellFor {
    # Packages defined in this project
    packages = ps: with ps; [
      elemental
    ];

    # Enable Hoogle documentation
    # This allows searching the installed packages, which is more practical
    # than the public Hoogle servers, since it only shows relevant results.
    withHoogle = true;

    tools = {
      cabal = "3.2.0.0";
    };

    buildInputs = with hsPkgs.pkgs; [
      # LLVM CLI tools for local testing purposes
      llvm-config
    ];

    # Fail if a needed package isn't installed by Nix
    # Otherwise, Cabal will try to build missing packages itself.
    exactDeps = true;
  }
