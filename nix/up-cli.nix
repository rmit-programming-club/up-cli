{ mkDerivation, aeson, base, bytestring, hpack, lens, stdenv, time
, wreq
}:
mkDerivation {
  pname = "up-cli";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base bytestring lens time wreq ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring lens time wreq
  ];
  testHaskellDepends = [ aeson base bytestring lens time wreq ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/up-cli#readme";
  license = stdenv.lib.licenses.bsd3;
}
