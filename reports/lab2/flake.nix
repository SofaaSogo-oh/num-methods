{
  description = "Numerical methods labs report";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils, ...}: 
  with flake-utils.lib; eachSystem allSystems (system: 
    let 
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      inherit (pkgs) stdenvNoCC makeFontsConf coreutils;
      fonts = makeFontsConf { 
        fontDirectories = with pkgs; [ 
            (nerdfonts.override { fonts = [ "Gohu" "3270" "JetBrainsMono" ]; })
            corefonts liberation_ttf 
        ]; 
      };
      tex = with pkgs; [
        (texlive.combine {
          inherit (pkgs.texlive) scheme-small latex-bin latexmk titlesec minted multirow cleveref mathtools babel-russian hyphenat;
        })
        python39Packages.pygments
      ];
      latex-command = "xelatex -halt-on-error -enable-installer -shell-escape";
    in rec {
      packages = {
        document = stdenvNoCC.mkDerivation {
          name = "test-latex-prj";
          src = self;
          FONTCONFIG_FILE = fonts;
          buildInputs = [ coreutils tex ];
          phases = ["unpackPhase" "buildPhase" "installPhase"];
          buildPhase = ''
              export TEXMFHOME=.cache
              export TEXMFVAR=.cache/texmf-var
              mkdir -p .cache/texmv-var
              ${latex-command} document.tex && ${latex-command} document.tex
          '';
          installPhase = ''
            mkdir -p $out
            cp document.pdf $out/
          '';
        };
      };
      defaultPackage = packages.document;
    });
}
