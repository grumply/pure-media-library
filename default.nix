{ mkDerivation, stdenv, ghc, base, pure-elm, pure-txt, pure-intersection, pure-json, pure-lifted, pure-marker, pure-readfile, pure-stream, pure-sync, pure-time, pure-websocket, pure-sorcerer, directory, filepath, hashable }:
mkDerivation {
  pname = "pure-media-library";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-elm
    pure-txt
    pure-intersection
    pure-json
    pure-lifted
    pure-marker
    pure-readfile
    pure-stream
    pure-sync
    pure-time
    pure-websocket
    pure-sorcerer
    directory
    filepath
    hashable
  ];
  homepage = "github.com/grumply/pure-media-library";
  license = stdenv.lib.licenses.bsd3;
}
