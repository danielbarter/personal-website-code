with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz") {});
let hpkgs = p: [ p.base
                 p.directory
                 p.filepath
                 p.cryptohash-md5
                 p.bytestring
                 p.text
                 p.pandoc
                 p.cabal-install
                 p.haskell-language-server
                 p.implicit-hie
               ];
in mkShell {
  buildInputs = [ texlive.combined.scheme-medium
                  (haskellPackages.ghcWithPackages hpkgs)
                ];
}
