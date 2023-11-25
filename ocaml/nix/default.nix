let
  pins = import ../npins/default.nix;
  config = { allowUnfree = true; };
  pkgs = pins.nixpkgs;
  nix-ocaml = pins.nix-overlays;
  overlays = [ (import "${nix-ocaml}/overlay" pkgs) ];
in
  import pkgs { inherit overlays config; }
