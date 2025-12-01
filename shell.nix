{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/871b9fd269ff6246794583ce4ee1031e1da71895.tar.gz) {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [ nodejs yarn curl jq ];
    shellHook = ''
        echo "Welcome to to the nix-shell for engineering.iog.io"
        echo "Run "yarn start", to launch the local development preview"
        function build() {
            yarn install --frozen-lockfile
            yarn build
        }
    '';
}
