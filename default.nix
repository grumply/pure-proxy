{ mkDerivation, base, pure, pure-prop, stdenv }:
mkDerivation {
  pname = "pure-proxy";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-prop ];
  homepage = "github.com/grumply/pure-proxy";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}
