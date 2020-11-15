with (import <nixpkgs> {});

let project = haskellPackages.callCabal2nix "personal-website-code" ./. {};
in mkShell {
  buildInputs = project.env.nativeBuildInputs ++
                [haskellPackages.cabal-install
                 texlive.combined.scheme-medium
                ];
}
