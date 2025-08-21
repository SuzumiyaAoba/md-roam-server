{
  description = "md-roam HTTP server development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Emacs configuration with required packages
        emacsWithPackages = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
          org-roam
          web-server
          json-mode
          markdown-mode
          dash
          s
          f
          ht
          request
        ]);
        
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacsWithPackages
            sqlite
            ripgrep
            fd
            git
            curl
            jq
          ];

          shellHook = ''
            echo "md-roam HTTP server development environment"
            echo "Emacs with org-roam and web-server packages available"
            echo ""
            echo "Available commands:"
            echo "  emacs - Start Emacs with org-roam and web-server"
            echo "  sqlite3 - SQLite database CLI"
            echo "  curl - Test HTTP endpoints"
            echo "  jq - JSON processor"
          '';
        };

        packages.default = emacsWithPackages;
      });
}