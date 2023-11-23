{ system ? builtins.currentSystem
, pins ? import ./npins
, pkgs ? import pins.nixpkgs { inherit system; }
, hooks ? import pins."pre-commit-hooks.nix"
}:

let
  inherit (pkgs) haskell;

  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${pkgs.ghcid}/bin/ghcid --clear \
    --command "cabal repl aoc2023:test:tests" \
    --test "hspec spec" \
    --setup "import Test.Hspec" \
    --restart=./src --restart=./test
  '';

  hsPkgs = pkgs.haskellPackages.extend (haskell.lib.packageSourceOverrides {
    aoc2023 = ./.;
  });

in
hsPkgs.shellFor {
  withHoogle = true;
  packages = ps: with ps; [ aoc2023 ];
  nativeBuildInputs = with pkgs; [
    nixpkgs-fmt
    ghc
    cabal-install
    ormolu
    hlint
    haskell-language-server
    pcre
    ghcid
    haskellPackages.cabal-fmt
    haskellPackages.haskell-ci
    watch-tests
  ];
}
