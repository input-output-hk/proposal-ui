let
  pkgs = import ./nix {};
in
  (import ./.).env.overrideAttrs(oldAttrs: oldAttrs // {
    buildInputs = oldAttrs.buildInputs ++ (with pkgs.haskellPackages; [
      ghcid
      cabal-install
    ]);
  })
