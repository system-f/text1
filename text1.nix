{ mkDerivation, base, binary, HUnit, lens, QuickCheck, semigroups
, stdenv, text
}:
mkDerivation {
  pname = "text1";
  version = "0.0.6.1";
  src = ./.;
  libraryHaskellDepends = [ base binary lens semigroups text ];
  testHaskellDepends = [
    base HUnit lens QuickCheck semigroups text
  ];
  homepage = "https://github.com/qfpl/text1";
  description = "Non-empty values of `Data.Text`.";
  license = stdenv.lib.licenses.bsd3;
}
