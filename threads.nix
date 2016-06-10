{ mkDerivation, base, concurrent-extra, HUnit, stdenv, stm
, test-framework, test-framework-hunit
}:
mkDerivation {
  pname = "threads";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base stm ];
  testHaskellDepends = [
    base concurrent-extra HUnit stm test-framework test-framework-hunit
  ];
  homepage = "https://github.com/basvandijk/threads";
  description = "Fork threads and wait for their result";
  license = stdenv.lib.licenses.bsd3;
}
