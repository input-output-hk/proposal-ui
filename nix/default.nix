{ sources ? import ./sources.nix
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
}:

import sources.nixpkgs {
  inherit system crossSystem;
  config = config // {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.extend (final: prev: {
        prometheus = pkgs.haskell.lib.doJailbreak prev.prometheus;
        amazonka-core = pkgs.haskell.lib.appendPatch prev.amazonka-core ./amazonka-content-length.patch;
      });
    };
  };
}
