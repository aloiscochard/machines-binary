{ mkDerivation, base, binary, bytestring, machines, stdenv }:
mkDerivation {
  pname = "machines-binary";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring machines ];
  homepage = "http://github.com/aloiscochard/machines-binary";
  description = "Binary utilities for the machines library";
  license = stdenv.lib.licenses.asl20;
}
