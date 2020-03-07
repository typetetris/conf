let
  all-hies = import (fetchTarball {
    url    = "https://github.com/infinisil/all-hies/tarball/92148680060ed68f24738128d8489f4f9387d2ff";
    sha256 = "1yb75f8p09dp4yx5d3w3wvdiflaav9a5202xz9whigk2p9ndwbp5";
  }) {};
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
    ssh_priv_key_file: /home/wolferic/.ssh/id_ed25519
    ssh_remote_host: tilia
    ssh_remote_port: 22
    ssh_remote_user: jenkins
    '';
in
  {
    allowUnfree = true;
    packageOverrides = pkgs: with pkgs; rec {

      myEmacsConfig = writeText "default.el"
        ''
          (global-set-key (kbd "C-c l") 'org-store-link)
          (global-set-key (kbd "C-c a") 'org-agenda)
          (global-set-key (kbd "C-c c") 'org-capture)
        '';
      myEmacs = emacsWithPackages (epkgs: (
        [
          (runCommand "default.el" {}
           ''
             mkdir -p $out/share/emacs/site-lisp
             cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
           '')
          epkgs.org
        ] ++ (with epkgs.melpaStablePackages; [
        ])));

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
      robotoTestcase = pkgs.buildEnv {
        name = "robotoTestcase";
        paths = [
          roboto-mono
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
          xlibs.xdpyinfo
          b9
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
          shellcheck
          idea.idea-community
          jdk
          kotlin
          stack
          jq
          myEmacs
#          haskellPackages.ormolu
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
          virtmanager
          libvirt.out
          screen-message
          vanilla-dmz
          polybar
          read-edid
          meld
          p7zip
          postgresql_9_6
          maven
          dbeaver
          gnumake
          volumeicon
          (import <nixos-unstable> {}).riot-desktop
          (
            let
              ghcversion = "ghc${builtins.replaceStrings ["."] [""] haskellPackages.ghc.version}";
            in
              all-hies.selection { selector = p: { "${ghcversion}" = p."${ghcversion}"; }; }
          )
            
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
