{ pkgs ? import <nixpkgs> {} }:
let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs:
  with epkgs.melpaPackages; [
    evil
    evil-tutor
    lsp-mode
    lsp-ui
    lsp-haskell
    helm
    which-key
    org-bullets
  ] ++ [ epkgs.orgPackages.org ] )
