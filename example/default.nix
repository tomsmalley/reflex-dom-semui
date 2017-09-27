{ mkDerivation, base, bytestring, containers, data-default
, file-embed, ghc-prim, ghcjs-dom, haskell-src-exts
, haskell-src-meta, hscolour, jsaddle, jsaddle-warp, lens, mtl
, reflex, reflex-dom-core, semantic-reflex, stdenv, template-haskell, text, these
, wai, wai-app-static, warp, websockets, ghc
}:
mkDerivation {
  pname = "example";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path)
    [ "makedocs.sh" "dist" ]
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
    semantic-reflex
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
