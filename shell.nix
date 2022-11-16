with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ocaml
        ocamlPackages.ocp-indent
        python3Packages.flake8
    ];
    shellHook = ''
        . .shellhook
    '';
}
