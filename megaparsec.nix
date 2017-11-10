{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, fetchgit, hspec, hspec-expectations, mtl
, parser-combinators, QuickCheck, scientific, stdenv, text
, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "6.2.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/megaparsec.git";
    sha256 = "1zq94vjscmbznshffrpllc45m32pk3j5iqiy5h7ahsdj3c8g5m6c";
    rev = "b99174d7f1c1b07257a0b67b941e4732b00bf13c";
  };
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring containers hspec hspec-expectations mtl QuickCheck
    scientific text transformers
  ];
  benchmarkHaskellDepends = [ base criterion deepseq text weigh ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
