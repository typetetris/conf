let
  all-hies = import (fetchTarball {
    url    = "https://github.com/infinisil/all-hies/tarball/92148680060ed68f24738128d8489f4f9387d2ff";
    sha256 = "1yb75f8p09dp4yx5d3w3wvdiflaav9a5202xz9whigk2p9ndwbp5";
  }) {};
in
{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    myNeovim = neovim.override {
      configure = {
        customRC = ''
          set hidden
          set termguicolors
          set diffopt=vertical,filler
          colorscheme base16-classic-light
          autocmd FileType haskell setlocal expandtab
          set list
          set listchars=tab:>-
          let g:ale_sign_column_always=1
          set expandtab
          let g:ale_echo_msg_format = '[%linter%] %s'
          let g:ale_linters = { 'haskell': ['hie'] }
        '';
        packages.myVimPackage = with pkgs.vimPlugins; {
          # see examples below how to use custom packages
          start = [ fugitive base16-vim vim-nix ale ];
          # If a vim plugin has a dependency that is not explicitly listed in
          # opt that dependency will always be added to start to avoid confusion.
          opt = [ ];
        };
      };
    };
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        bc
        firefox
        chromium
        gitFull
        keepassxc
        killall
        mkpasswd
        mupdf
        ripgrep
        simple-scan
        thunderbird
        usbutils
        yubikey-manager-qt
        yubikey-personalization
        yubikey-personalization-gui
        yubico-pam
        myNeovim
        gnupg
        pavucontrol
        networkmanagerapplet
        shellcheck
        idea.idea-community
        jdk
        kotlin
        stack
        jq
        haskellPackages.ormolu
        glxinfo
        htop
        cabal2nix
        man-pages
        pciutils
        roboto-mono
        unzip
        vscode
        xsel
        (
          let
            ghcversion = "ghc${builtins.replaceStrings ["."] [""] haskellPackages.ghc.version}";
          in
            all-hies.selection { selector = p: { "${ghcversion}" = p."${ghcversion}"; }; }
        )
          
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" "info" ];
      postBuild = ''
        if [ -x $out/bin/install-info -a -w $out/share/info ]; then
          shopt -s nullglob
          for i in $out/share/info/*.info $out/share/info/*.info.gz; do
              $out/bin/install-info $i $out/share/info/dir
          done
        fi
      '';
    };
  };
}
