let
  sources = import ./nix/sources.nix;
  b9ConfigFile =
    ''
      [global]
      build_dir_root: Nothing
      default_timeout_seconds: 3600
      keep_temp_dirs: False
      log_file: Nothing
      max_cached_shared_images: Just 2
      repository: Nothing
      repository_cache: Just (InB9UserDir "repo-cache")
      unique_build_dirs: True
      verbosity: Just LogTrace

      [libvirt-lxc]
      connection: lxc:///
      emulator_path: Just "/run/libvirt/nix-emulators/libvirt_lxc"
      guest_capabilities: [CAP_MKNOD,CAP_SYS_ADMIN,CAP_SYS_CHROOT,CAP_SETGID,CAP_SETUID,CAP_NET_BIND_SERVICE,CAP_SETPCAP,CAP_SYS_PTRACE,CAP_SYS_MODULE]
      guest_ram_size: RamSize 4 GB
      image_file_names_shortener_base_path: Just "/home/tmp"
      network: Nothing
      use_sudo: False

      [systemdNspawn]
      console: read-only
      executable: Just "/nix/store/grsj02sxnkgsa1hgl9rnhcwnd5p96gi7-systemd-243.7/bin/systemd-nspawn"
      extra_args: Nothing
      guest_capabilities: [CAP_MKNOD,CAP_SYS_ADMIN,CAP_SYS_CHROOT,CAP_SETGID,CAP_SETUID,CAP_NET_BIND_SERVICE,CAP_SETPCAP,CAP_SYS_PTRACE,CAP_SYS_MODULE]
      max_lifetime_seconds: Just 14400
      use_sudo: False

      [tilia-repo]
      remote_path: /var/lib/jenkins/.b9/repo-cache/local-repo
      ssh_priv_key_file: /home/wolferic/.ssh/id_ed25519
      ssh_remote_host: tilia
      ssh_remote_port: 22
      ssh_remote_user: jenkins
    '';
  hydra-check = fetchzip: import (fetchzip {
    url = sources.hydra-check.url;
    sha256 = sources.hydra-check.sha256;
  });
in
{
  permittedInsecurePackages = [
    "adobe-reader-9.5.5-1"
  ];

  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    myNeovim = pkgs.neovim.override {
      configure = {
        customRC =
          ''
            packloadall
            " TextEdit might fail if hidden is not set.
            set hidden

            " for a sane search feature
            set ignorecase
            set smartcase

            " Some servers have issues with backup files, see #649.
            set nobackup
            set nowritebackup

            " sane search
            set ignorecase
            set smartcase

            " Give more space for displaying messages.
            set cmdheight=2

            " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
            " delays and poor user experience.
            set updatetime=300

            " Don't pass messages to |ins-completion-menu|.
            set shortmess+=c

            " Always show the signcolumn, otherwise it would shift the text each time
            " diagnostics appear/become resolved.
            set signcolumn=yes

            set termguicolors
            set diffopt=vertical,filler
            " colorscheme base16-classic-light
            colorscheme PaperColor
            let g:airline_theme='base16_default_dark'
            " hack, but all the other stuff didn't work
            autocmd VimEnter * AirlineTheme base16_default_dark
            set list
            set listchars=tab:>-
            set expandtab
            set shiftwidth=2
            set softtabstop=2
            set tabstop=2

            " denite configuration
            call denite#custom#var('file/rec', 'command', ['${pkgs.ripgrep}/bin/rg', '--files', '--glob', '!.git'])

            call denite#custom#var('grep', 'command', ['${pkgs.ripgrep}/bin/rg'])
            call denite#custom#var('grep', 'default_opts',
            \ ['-i', '--vimgrep', '--no-heading'])
            call denite#custom#var('grep', 'recursive_opts', [])
            call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
            call denite#custom#var('grep', 'separator', ['--'])
            call denite#custom#var('grep', 'final_opts', [])
            call denite#custom#option('default', 'prompt', 'λ')


            nmap <leader>p :Denite -start-filter file/rec<CR>
            nmap <leader>b :Denite buffer<CR>
            nnoremap <leader>g :Denite grep<CR>
            nnoremap <leader>/ :DeniteCursorWord grep<CR>

            augroup mygroup2
            autocmd!
            autocmd FileType denite nnoremap <silent><buffer><expr> <CR>  denite#do_map('do_action')
            autocmd FileType denite nnoremap <silent><buffer><expr> d     denite#do_map('do_action', 'delete')
            autocmd FileType denite nnoremap <silent><buffer><expr> p     denite#do_map('do_action', 'preview')
            autocmd FileType denite nnoremap <silent><buffer><expr> <C-v> denite#do_map('do_action', 'vsplit')
            autocmd FileType denite nnoremap <silent><buffer><expr> <C-x> denite#do_map('do_action', 'split')
            autocmd FileType denite nnoremap <silent><buffer><expr> <Esc> denite#do_map('quit')
            autocmd FileType denite nnoremap <silent><buffer><expr> i     denite#do_map('open_filter_buffer')
            autocmd FileType denite nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
            autocmd FileType denite-filter imap <silent><buffer> <Esc> <Plug>(denite_filter_quit)
            augroup END

            if !exists("g:ormolu_command")
            let g:ormolu_command = "${pkgs.ormolu}/bin/ormolu"
            endif

            if !exists("g:nixpkgs_fmt_command")
            let g:nixpkgs_fmt_command = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
            endif

            "run ormolu on save
            function FormatBuffer()
              if 0 && &modified && &filetype ==? "haskell"
                silent !clear
                let cursor_pos = getcurpos()
                let formatted_content = systemlist(g:ormolu_command, getbufline("%",0,"$"))
                if v:shell_error ==? 0
                  execute '%!' . g:ormolu_command
                  call setpos('.', cursor_pos)
                else
                  echom ("Failed to format buffer: " . expand("%"))
                endif  
              elseif &modified && &filetype ==? "nix"
                silent !clear
                let cursor_pos = getcurpos()
                let formatted_content = systemlist(g:nixpkgs_fmt_command, getbufline("%",0,"$"))
                if v:shell_error ==? 0
                  execute '%!' . g:nixpkgs_fmt_command
                  call setpos('.', cursor_pos)
                else
                  echom ("Failed to format buffer: " . expand("%"))
                endif  
              endif
            endfunction

            augroup mygroup3
            autocmd!
            autocmd BufWritePre * :call FormatBuffer()
            augroup END

            set makeprg=cabal\ build\ --disable-optimization
            " mark undefined, error in error color
            syntax keyword indivError undefined error
            highlight link indivError Error
            augroup mygroup4
            autocmd!
            autocmd FileType haskell syntax keyword indivError undefined error
            augroup END

            """"""""""""""""""""""""""""""""""""""""""""""""
            " COC Config
            """"""""""""""""""""""""""""""""""""""""""""""""

            " Trigger completion on Ctrl+Space
            inoremap <silent><expr> <c-space> coc#refresh()

            " Use <CR> to confirm completion
            inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"
            
            " Set the coc-config
            call coc#config('languageserver', {
                    \  "haskell": {
                    \    "command": "haskell-language-server",
                    \    "args": ["--lsp"],
                    \    "rootPatterns": [ "*.cabal", ],
                    \    "filetypes": [ "hs", "lhs", "haskell" ],
                    \  },
                    \  "erlang": {
                    \    "command": "erlang_ls",
                    \    "rootPatterns": [ "rebar.config", ],
                    \    "filetypes": [ "erlang" ],
                    \  }
                    \})
            call coc#config('coc.preferences.currentFunctionSymbolAutoUpdate', 'true')
            call coc#config('rust-analyzer', {
                    \ "server": {"path":  "/home/wolferic/.cargo/bin/rust-analyzer"},
                    \ "checkOnSave": {"command": "clippy"}
                    \})

            nmap <leader>1 :call coc#config('diagnostic.messageTarget', 'echo')<CR>
            nmap <leader>2 :call coc#config('diagnostic.messageTarget', 'float')<CR>

            " Use `[g` and `]g` to navigate diagnostics
            nmap <silent> [g <Plug>(coc-diagnostic-prev)
            nmap <silent> ]g <Plug>(coc-diagnostic-next)

            " GoTo code navigation.
            nmap <silent> gd <Plug>(coc-definition)
            nmap <silent> gy <Plug>(coc-type-definition)
            nmap <silent> gi <Plug>(coc-implementation)
            nmap <silent> gr <Plug>(coc-references)

            " Use K to show documentation in preview window.
            nnoremap <silent> K :call <SID>show_documentation()<CR>

            function! s:show_documentation()
              if (index(['vim','help'], &filetype) >= 0)
                execute 'h '.expand('<cword>')
              else
                call CocAction('doHover')
              endif
            endfunction

            " Highlight the symbol and its references when holding the cursor.
            autocmd CursorHold * silent call CocActionAsync('highlight')

            " Symbol renaming.
            nmap <leader>rn <Plug>(coc-rename)

            " Formatting selected code.
            xmap <leader>f  <Plug>(coc-format-selected)
            nmap <leader>f  <Plug>(coc-format-selected)

            augroup mygroup
              autocmd!
              " Setup formatexpr specified filetype(s).
              autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
              " Update signature help on jump placeholder.
              autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
            augroup end

            " Applying codeAction to the selected region.
            " Example: `<leader>aap` for current paragraph
            xmap <leader>a  <Plug>(coc-codeaction-selected)
            nmap <leader>a  <Plug>(coc-codeaction-selected)

            " Remap keys for applying codeAction to the current line.
            nmap <leader>ac  <Plug>(coc-codeaction)
            " Apply AutoFix to problem on the current line.
            nmap <leader>qf  <Plug>(coc-fix-current)

            " Introduce function text object
            " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
            xmap if <Plug>(coc-funcobj-i)
            xmap af <Plug>(coc-funcobj-a)
            omap if <Plug>(coc-funcobj-i)
            omap af <Plug>(coc-funcobj-a)

            " Use <TAB> for selections ranges.
            " NOTE: Requires 'textDocument/selectionRange' support from the language server.
            " coc-tsserver, coc-python are the examples of servers that support it.
            nmap <silent> <TAB> <Plug>(coc-range-select)
            xmap <silent> <TAB> <Plug>(coc-range-select)

            " Add `:Format` command to format current buffer.
            command! -nargs=0 Format :call CocAction('format')

            " Add `:Fold` command to fold current buffer.
            command! -nargs=? Fold :call     CocAction('fold', <f-args>)

            " Add `:OR` command for organize imports of the current buffer.
            command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

            " Add (Neo)Vim's native statusline support.
            " NOTE: Please see `:h coc-status` for integrations with external plugins that
            " provide custom statusline: lightline.vim, vim-airline.
            " set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}
            " set statusline+=%F

            function! CocCurrentFunction()
              return get(b:,'coc_current_function',''')
            endfunction

            function! CocExtensionStatus()
              return get(g:,'coc_status',''')
            endfunction

            " Mappings using CoCList:
            " Show all diagnostics.
            nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
            " Manage extensions.
            nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
            " Show commands.
            nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
            " Find symbol of current document.
            nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
            " Search workspace symbols.
            nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
            " Do default action for next item.
            nnoremap <silent> <space>j  :<C-u>CocNext<CR>
            " Do default action for previous item.
            nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
            " Resume latest coc list.
            nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
          '';
        packages.myVimPackage = {
          # see examples below how to use custom packages
          start =
            with pkgs.vimPlugins;
            [
              fugitive
              vim-nix
              #lightline-vim
              vim-airline
              vim-airline-themes
              coc-nvim
              coc-rust-analyzer
              denite
              denite-nvim
              denite-extra
              papercolor-theme
              vim-json
              vim-yaml
              multiple-cursors
              ghcid
              base16-vim
            ];
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

    b9ConfigFileS = writeText "myB9ConfigFile" b9ConfigFile;


    myPackages =
      let b9 = import
        (pkgs.fetchFromGitHub {
          owner = "sheyll";
          repo = "b9-vm-image-builder";
          rev = "0.5.69.0";
          sha256 = "1c37haid9nwzqa41brrqirqpgzbynfmfm88l272nijc0pqgik9bj";
        })
        { inherit pkgs; };
      in
      pkgs.buildEnv {
        name = "myPackages";
        paths = [
          (runCommand "b9ConfigFileS" { } ''
            mkdir -p $out/etc/b9
            cp ${b9ConfigFileS} $out/etc/b9/b9.conf
          '')
          #            (runCommand "b9ConfigFileS" {} ''
          #              mkdir -p $out/etc/b9
          #              cp ${b9ConfigFileHostnetS} $out/etc/b9/b9.conf.hostnet
          #            '')
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
          #b9
          xlibs.xdpyinfo
          bc
          firefox
          gitFull
          keepassxc
          killall
          mkpasswd
          mupdf
          #  (texlive.combine {inherit (texlive) scheme-medium;})
          ripgrep
          simple-scan
          thunderbird
          usbutils
          yubikey-manager-qt
          yubikey-personalization
          yubikey-personalization-gui
          yubico-pam
          myNeovim
          yubioath-desktop
          gnupg
          pinentry-qt
          qtpass
          pavucontrol
          shellcheck
          jdk
          idea.idea-ultimate
          idea.clion
          kotlin
          stack
          libvirt.out
          glxinfo
          htop
          cabal2nix
          man-pages
          pciutils
          unzip
          xsel
          wget
          xorg.xev
          arandr
          libreoffice
          rofi
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
          x11vnc
          zoom-us
          nox
          nodejs
          #  (hydra-check pkgs.fetchzip { pkgs = pkgs; })
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
