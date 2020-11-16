with (import <nixpkgs> {});

let pkgs = p: [ p.base
                p.directory
                p.filepath
                p.cryptohash-md5
                p.bytestring
                p.text
                p.pandoc
              ];
in mkShell {
  buildInputs = [ texlive.combined.scheme-medium
                  (haskellPackages.ghcWithPackages pkgs)
                  haskellPackages.cabal-install
                ];
}
