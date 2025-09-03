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
        
        # md-roam package from GitHub
        md-roam = pkgs.emacs.pkgs.melpaBuild {
          pname = "md-roam";
          version = "1.0.0";
          src = pkgs.fetchFromGitHub {
            owner = "nobiot";
            repo = "md-roam";
            rev = "1113a568138c1e1084a3cd41a04a9cff2ff14a72";
            sha256 = "sha256-YxkL6vqabh2qkmgH2zUNFhUoQBQ07sjj9bFdFrWGlf0=";
          };
          packageRequires = with pkgs.emacs.pkgs; [ org-roam markdown-mode ];
          meta = {
            description = "Roam Research replica with Org-mode and Markdown";
            homepage = "https://github.com/nobiot/md-roam";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

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
          md-roam
          async
          yaml-mode
          yaml
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
            echo ""
          '';
        };

        packages = {
          default = emacsWithPackages;
        };
      });
}