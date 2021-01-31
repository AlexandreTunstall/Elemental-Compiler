{ # Fetch haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball {
    name = "master-2021.01.31";
    url = "https://github.com/input-output-hk/haskell.nix/archive/2e38dc1cd7ffc1e473e95e998c22ff031ea93f79.tar.gz";
    sha256 = "0s8mp9bdln27v8xvwhcb16vcwx99hm9qbnibsi9k2q9xvwi5p8fv";
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
