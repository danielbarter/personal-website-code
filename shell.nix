with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.03.tar.gz") {});
let hpkgs = p: [ p.base
                 p.directory
                 p.filepath
                 p.cryptohash-md5
                 p.bytestring
                 p.text
                 p.pandoc
               ];
in mkShell {
  buildInputs = [ texlive.combined.scheme-medium
                  (haskellPackages.ghcWithPackages hpkgs)
                  haskellPackages.cabal-install
                ];
}
