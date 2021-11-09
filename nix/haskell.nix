{ # Fetch haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball {
    name = "master-2021.10.27";
    url = "https://github.com/input-output-hk/haskell.nix/archive/b18c6ce0867fee77f12ecf41dc6c67f7a59d9826.tar.gz";
    sha256 = "185jipix59gn176sm8pp9pdlpm6mdmbxg4iklnwbj64a8qsc4npd";
  }) {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
# you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2111

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
