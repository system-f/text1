{ mkDerivation, base, binary, Cabal, cabal-doctest, directory
, doctest, filepath, lens, papa, QuickCheck, semigroups, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "text1";
  version = "0.0.5";
  src = ./.;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base binary lens papa semigroups text ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck template-haskell
  ];
  homepage = "https://github.com/qfpl/text1";
  description = "Non-empty values of `Data.Text`.";
  license = stdenv.lib.licenses.bsd3;
}
