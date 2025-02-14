with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-24.11.tar.gz") {});
let hpkgs = p: [ p.base
                 p.directory
                 p.filepath
                 p.cryptohash-md5
                 p.bytestring
                 p.text
                 p.pandoc
                 p.cabal-install
               ];
in mkShell {
  buildInputs = [ texlive.combined.scheme-medium
                  (haskellPackages.ghcWithPackages hpkgs)
                ];
}
