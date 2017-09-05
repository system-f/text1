{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "f76316938c1d5398cc9ab01de12bfde6b68b8502";
      sha256 = "14dlk0v2p6y65953bnmy6bm3m5a5fag209r53kq6ycwqw8m0m1ja";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {};
  };

  text1 = haskellPackages.callPackage ./text1.nix {};

in
  text1
