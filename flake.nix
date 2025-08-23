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

        # org-roam-ui source with web assets
        org-roam-ui-src = pkgs.fetchFromGitHub {
          owner = "org-roam";
          repo = "org-roam-ui";
          rev = "5ac74960231db0bf7783c2ba7a19a60f582e91ab";
          sha256 = "sha256-dCoEQRi86eMerkMQPy3Ow/Kj9kzHxXRSrDk4cl8uLHo=";
        };
        
        # Extract org-roam-ui web assets to local directory
        org-roam-ui-web = pkgs.runCommand "org-roam-ui-web" {} ''
          mkdir -p $out
          cp -r ${org-roam-ui-src}/out $out/
          echo "org-roam-ui web assets extracted to: $out/out"
        '';
        
        org-roam-ui = pkgs.emacs.pkgs.melpaBuild {
          pname = "org-roam-ui";
          version = "1.0.0";
          src = org-roam-ui-src;
          packageRequires = with pkgs.emacs.pkgs; [ org-roam simple-httpd websocket ];
          
          postInstall = ''
            # Copy web assets to package directory
            if [ -d $src/out ]; then
              mkdir -p $out/share/emacs/site-lisp/elpa/org-roam-ui-1.0.0/out
              cp -r $src/out/* $out/share/emacs/site-lisp/elpa/org-roam-ui-1.0.0/out/
            fi
          '';
          
          meta = {
            description = "A graphical frontend for exploring your org-roam Zettelkasten";
            homepage = "https://github.com/org-roam/org-roam-ui";
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
          simple-httpd
          websocket
          org-roam-ui
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
            org-roam-ui-web
          ];

          shellHook = ''
            echo "md-roam HTTP server development environment"
            echo "Emacs with org-roam and web-server packages available"
            echo ""
            
            # Setup org-roam-ui web assets
            if [ ! -d "org-roam-ui-web" ] || [ ! -f "org-roam-ui-web/out/index.html" ]; then
              echo "Setting up org-roam-ui web assets..."
              if [ -d "org-roam-ui-web" ]; then
                rm -rf org-roam-ui-web
              fi
              ln -sf ${org-roam-ui-web} org-roam-ui-web
              echo "✅ org-roam-ui web assets linked successfully"
            else
              echo "✅ org-roam-ui web assets already available"
            fi
            
            echo ""
            echo "Available commands:"
            echo "  emacs - Start Emacs with org-roam and web-server"
            echo "  sqlite3 - SQLite database CLI"
            echo "  curl - Test HTTP endpoints"
            echo "  jq - JSON processor"
            echo ""
            echo "Web assets location: ./org-roam-ui-web/out/"
          '';
        };

        packages = {
          default = emacsWithPackages;
          org-roam-ui-web = org-roam-ui-web;
        };
      });
}