{ mkDerivation, base, binary, directory, doctest, filepath, lens
, QuickCheck, semigroups, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "text1";
  version = "0.0.5";
  src = ./.;
  libraryHaskellDepends = [ base binary lens semigroups text ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck template-haskell
  ];
  homepage = "https://github.com/qfpl/text1";
  description = "Non-empty values of `Data.Text`.";
  license = stdenv.lib.licenses.bsd3;
}
