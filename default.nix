{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/57a8be730371f9c5dd14451f0e0756b4d5a8b5fa.tar.gz";
  sha256 = "0ak6gprcnv9xnya9mavwanj3nn2d912x9wjkq19zp4rj3g2lcy0a";
}) {}
, compilerVersion ? "ghc8107"
, compiler ? pkgs.haskell.packages.${compilerVersion}
}:

compiler.developPackage {
  root = pkgs.lib.cleanSource ./.;

  modifier = drv: pkgs.haskell.lib.compose.addBuildTools [
    pkgs.haskellPackages.cabal-install
    pkgs.haskell-language-server
    # LLVM CLI tools for local testing purposes.
    pkgs.llvm_9
    # For viewing heap profiles (mainly ps2pdf).
    pkgs.ghostscript
  ] (pkgs.haskell.lib.compose.appendConfigureFlag "--ghc-options=-Werror" drv);
}
