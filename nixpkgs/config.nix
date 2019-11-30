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
        shutter
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
