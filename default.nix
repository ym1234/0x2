{ mkDerivation, base, bytestring, cryptohash-sha256, directory
, filepath, hexstring, scotty, stdenv, text, unix, unix-time, wai
, wai-extra
}:
mkDerivation {
  pname = "0x2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cryptohash-sha256 directory filepath hexstring
    scotty text unix unix-time wai wai-extra
  ];
  description = "A file host, like 0x0.st and ptpb";
  license = stdenv.lib.licenses.mit;
}
