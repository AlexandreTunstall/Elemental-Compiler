let
  hsPkgs = import ./default.nix {
    extraModules = [
      {
        packages.elemental.package.ghcOptions = "-Werror";
        packages.elemental.components.tests.test.testFlags = ["--xml" "$out/test-report.xml"];
      }
    ];
  };
in
  hsPkgs.project