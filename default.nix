{ reflex-platform ? ../reflex-platform/. }:
let

  platform = import reflex-platform { };

in rec {

  ghc = platform.ghc.callPackage ./reflex-dom-semui.nix {
    ghc = platform.ghc;
  };

  ghcjs = platform.ghcjs.callPackage ./reflex-dom-semui.nix {
    ghc = platform.ghcjs;
  };

}
