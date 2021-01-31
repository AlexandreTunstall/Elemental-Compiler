{ # Fetch haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball {
    name = "master-2021.01.07";
    url = "https://github.com/input-output-hk/haskell.nix/archive/8706c33717b566119e68e715e844645b01a6338b.tar.gz";
    sha256 = "13d19x2qy2qh1jn1cd8i9f8ld94cp7cr3y04f2yx1h64d1ya2rrg";
  }) {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
# you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

# haskell.nix provides some arguments to be passed to nixpkgs, including some patches
# and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# We may want to add additional overlays to improve build times.
, extraOverlays ? []

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc (nixpkgsArgs // {
    overlays = nixpkgsArgs.overlays ++ extraOverlays;
  })
}: {
  inherit pkgs haskellNix;
}
