# Legacy shell.nix for backward compatibility
# Use 'nix develop' with flake.nix for better experience
(import (
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    sha256 = "sha256:1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2";
  }
) {
  src = ./.;
}).shellNix