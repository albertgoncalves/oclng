with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_14.stdenv; } {
    buildInputs = [
        fasm
        mold
        musl
        ocaml
        ocamlPackages.ocp-indent
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
    hardeningDisable = [ "all" ];
}
