let
  b9ConfigFile = lxc_emulator: network:
    ''
    [global]
    build_dir_root: Nothing
    environment_vars: []
    exec_env: LibVirtLXC
    keep_temp_dirs: False
    log_file: Nothing
    profile_file: Nothing
    repository: Nothing
    repository_cache: Just (InB9UserDir "repo-cache")
    unique_build_dirs: True
    max_cached_shared_images: Nothing
    verbosity: Just LogTrace
    
    [libvirt-lxc]
    use_sudo: False
    emulator_path: Just "${lxc_emulator}"
    connection: lxc:///
    network: ${network}
    guest_capabilities: [CAP_MKNOD,CAP_SYS_ADMIN,CAP_SYS_CHROOT,CAP_SETGID,CAP_SETUID,CAP_NET_BIND_SERVICE,CAP_SETPCAP,CAP_SYS_PTRACE,CAP_SYS_MODULE]
    guest_ram_size: RamSize 6 GB
    
    [tilia-repo]
    remote_path: /var/lib/jenkins/.b9/repo-cache/local-repo
    ssh_priv_key_file: /home/typetetris/.ssh/id_lbm_ed25519
    ssh_remote_host: tilia
    ssh_remote_port: 22
    ssh_remote_user: jenkins
    '';
  hydra-check = import (fetchTarball {
    url = "https://github.com/nix-community/hydra-check/tarball/4b60c03fb43c78eca6a241384f3a50b36863d911";
    sha256 = "1pz5pc9an1v56cda95bw1jljr2fl3g01pjfkakpgzw538kdd7fb0";
  });
in
  {
      permittedInsecurePackages = [
         "adobe-reader-9.5.5-1"
       ];

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
            let g:ale_linters = { 'haskell': [] }
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

    myEmacsConfig = writeText "default.el"
    ''
      (eval-when-compile
        (require 'use-package))
      (use-package dante
       :ensure t
       :after haskell-mode
       :commands 'dante-mode
       :init
       (add-hook 'haskell-mode-hook 'dante-mode))
       
      (global-set-key (kbd "C-c l") 'org-store-link)
      (global-set-key (kbd "C-c a") 'org-agenda)
      (global-set-key (kbd "C-c c") 'org-capture)
    '';
#    myEmacs = emacsWithPackages (epkgs: (
#      [
#        (runCommand "default.el" {}
#        ''
#        mkdir -p $out/share/emacs/site-lisp
#        cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
#        '')
#        epkgs.org
#      ] ++ (with epkgs.melpaStablePackages; [
#        dante
#        haskell-mode
#        use-package
#      ])));

      b9ConfigFileS = writeText "myB9ConfigFile" (b9ConfigFile "${pkgs.libvirt.out}/libexec/libvirt_lxc" "Just \"default\"");
      b9ConfigFileHostnetS = writeText "myB9ConfigFile" (b9ConfigFile "${pkgs.libvirt.out}/libexec/libvirt_lxc" "Nothing");


      myPackages =
        let b9 = import
        (pkgs.fetchFromGitHub {
          owner = "sheyll";
          repo = "b9-vm-image-builder";
          rev = "0.5.69.0";
          sha256 = "1c37haid9nwzqa41brrqirqpgzbynfmfm88l272nijc0pqgik9bj";
        }) {inherit pkgs;};
        in
        pkgs.buildEnv {
          name = "myPackages";
          paths = [
            (runCommand "b9ConfigFileS" {} ''
              mkdir -p $out/etc/b9
              cp ${b9ConfigFileS} $out/etc/b9/b9.conf
            '')
            (runCommand "b9ConfigFileS" {} ''
              mkdir -p $out/etc/b9
              cp ${b9ConfigFileHostnetS} $out/etc/b9/b9.conf.hostnet
            '')
            element-desktop
            at
            bind
            calibre
            gcc
            hydra-cli
            inetutils
            mercurialFull
            poetry
#            pkgs.poetry2nix gives strange error but nix-env -iA nixos.poetry2nix works ... 
            pstree
            pwgen
            stunnel
            vlc
            xxd
            erlang
            b9
            xlibs.xdpyinfo
            bc
            firefox
            gitFull
            keepassxc
            killall
            mkpasswd
            mupdf
            (texlive.combine {inherit (texlive) scheme-medium;})
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
            pinentry-qt
            qtpass
            pavucontrol
            shellcheck
            jdk
            idea.idea-community
            kotlin
            stack
            libvirt.out
            jq
            glxinfo
            htop
            cabal2nix
            man-pages
            pciutils
            unzip
            vscode
            xsel
            wget
            xorg.xev
            xscreensaver
            arandr
            libreoffice
            rofi
            screen-message
            vanilla-dmz
            read-edid
            meld
            p7zip
            postgresql_9_6
            maven
            dbeaver
            gnumake
            volumeicon
            chromium
        gcr
        liburcu
        openssl
        krb5
        icu
        zlib
        libsecret
        desktop-file-utils
        xorg.xprop
        sshfs
        gimp
        shutter
        adobe-reader
        file
        nfs-utils
        pass
        teamviewer
        tigervnc
        virt-manager
        x11vnc
        zoom-us
        nox
        (hydra-check { pkgs = pkgs; })
            ];
            pathsToLink = [ "/share" "/bin" "/etc" ];
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
