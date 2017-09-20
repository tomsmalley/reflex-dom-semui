{ mkDerivation, base, bytestring, containers, data-default, file-embed
, ghcjs-dom, haskell-src-exts, haskell-src-meta, hscolour, jsaddle, jsaddle-warp
, lens, mtl, reflex, reflex-dom-core, stdenv, template-haskell
, text, wai-app-static, warp, websockets, ghc
}:
mkDerivation rec {
  pname = "reflex-dom-semui";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path)
    [ "makedocs.sh" ".git" "dist" "docs" ]
  )) ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base
    bytestring
    containers
    data-default
    file-embed
    ghcjs-dom
    haskell-src-exts
    haskell-src-meta
    hscolour
    jsaddle
    lens
    mtl
    reflex
    reflex-dom-core
    template-haskell
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
