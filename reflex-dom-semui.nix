{ mkDerivation, base, containers, data-default, file-embed
, ghcjs-dom, jsaddle, jsaddle-warp, lens, mtl, reflex
, reflex-dom-core, stdenv, text, wai-app-static, warp
, websockets, ghc
}:
mkDerivation rec {
  pname = "reflex-dom-semui";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path)
    [ ".git" "dist" "docs" ]
  )) ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base
    containers
    data-default
    file-embed
    ghcjs-dom
    jsaddle
    lens
    mtl
    reflex
    reflex-dom-core
    text
  ] ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.isDarwin then [
  ] else [
    jsaddle-warp
    wai-app-static
    warp
    websockets
  ]);
  description = "A reflex-dom API for semantic-ui components";
  license = stdenv.lib.licenses.bsd3;
}
