{ cabal, concurrentExtra, HUnit, stm
, testFramework, testFrameworkHunit
}:

cabal.mkDerivation (self: {
  pname = "threads";
  version = "HEAD";
  src = ./.;
  buildDepends = [ stm ];
  testDepends = [
    concurrentExtra HUnit stm testFramework testFrameworkHunit
  ];
  meta = {
    homepage = "https://github.com/basvandijk/threads";
    description = "Fork threads and wait for their result";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
