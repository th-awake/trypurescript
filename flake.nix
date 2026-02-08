{
  description = "Try PureScript - Interactive PureScript in the Browser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, purescript-overlay }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems
          (system: f (import nixpkgs { inherit system; overlays = [ purescript-overlay.overlays.default overlay ]; }));

      purescript-src = {
        owner = "purescript";
        repo = "purescript";
        rev = "8ac0fb2962a7df318a74216872465dc2868c6064";
        hash = "sha256-m78A3dABehfxBSQszhedZdAHtPAVlGxfOmD0669JPMU=";
      };

      cheapskate-src = {
        owner = "purescript";
        repo = "cheapskate";
        rev = "633c69024e061ad956f1aecfc137fb99a7a7a20b";
        hash = "sha256-kVwAyWbpvKH5l/TGZEaBGMHE7mw53616TPJnDlNND3U=";
      };

      overlay = final: prev: {
        trypurescript-haskellPackages = prev.haskell.packages.ghc984.override {
          overrides = hfinal: hprev: {
            purescript = final.haskell.lib.compose.doJailbreak (hfinal.callCabal2nix "purescript"
              (final.fetchFromGitHub purescript-src) {});

            cheapskate = final.haskell.lib.compose.doJailbreak (hfinal.callCabal2nix "cheapskate"
              (final.fetchFromGitHub cheapskate-src) {});

            language-javascript = hprev.language-javascript_0_7_0_0;

            trypurescript = hfinal.callCabal2nix "trypurescript" ./. {};
          };
        };

        trypurescript =
          final.haskell.lib.compose.overrideCabal
            (_: { disallowGhcReference = false; })
            (final.haskell.lib.compose.justStaticExecutables
              final.trypurescript-haskellPackages.trypurescript);

        trypurescript-dev-shell = final.trypurescript-haskellPackages.shellFor {
          packages = ps: [ ps.trypurescript ];
          nativeBuildInputs = [
            final.cabal-install
            final.stack
            final.pkg-config
            final.purs
            final.spago
            final.esbuild
            final.nodejs
          ];
          buildInputs = [
            final.zlib
          ];
        };
      };
    in
    {
      overlays.default = overlay;

      packages = forAllSystems (pkgs: {
        default = pkgs.trypurescript;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.trypurescript-dev-shell;
      });
    };
}
