{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-eopl3";

          packages = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
          ];

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name)\n$PS1"

            f () {
              elm-format "$PROJECT_ROOT/"{src,tests} "''${@:---yes}"
            }

            t () {
              elm-test "$@"
            }

            c () {
              nix flake check -L
              f --validate
              t
            }

            clean () {
              rm -rf "$PROJECT_ROOT/elm-stuff"
            }

            echo "Development environment loaded"
            echo ""
            echo "Type 'f' to run elm-format"
            echo "Type 't' to run elm-test"
            echo "Type 'c' to run all checks"
            echo "Type 'clean' to remove build artifacts"
            echo ""
          '';
        };
      }
    );
}
