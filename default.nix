{ reflex-platform ? ../reflex-platform/. }:
let

  dontCheckPackages = [
    "lens"
  ];

  pkgs = import <nixpkgs> { };
  platform = import reflex-platform { };

  makeOverrides = function: names: self: super:
    let toPackage = name: {
          inherit name;
          value = function super.${name};
        };
    in builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: { });
  doOverrides = haskellPackages: haskellPackages.override {
    overrides = composeExtensionsList [
      generatedOverrides
      (makeOverrides pkgs.haskell.lib.dontCheck dontCheckPackages)
    ];
  };
  generatedOverrides = self: super:
    let toPackage = file: _: {
          name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
          value = self.callPackage (./. + "/nix/${file}") { };
        };
    in pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

in rec {

  semantic-reflex = (doOverrides platform.ghc).callPackage ./semantic-reflex {
    ghcjs = false;
  };

  semantic-reflex-js = (doOverrides platform.ghcjs).callPackage ./semantic-reflex {
    ghcjs = true;
  };
  ghc = (doOverrides platform.ghc).callPackage ./example {
    ghc = platform.ghc;
    inherit semantic-reflex;
  };

  ghcjs = (doOverrides platform.ghcjs).callPackage ./example {
    ghc = platform.ghcjs;
    semantic-reflex = semantic-reflex-js;
  };

}
