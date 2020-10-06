let
  pkgs = import ./nix {};
in
  pkgs.haskellPackages.callCabal2nix "proposal-ui" ./. {}
