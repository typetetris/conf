# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = [
      (self: super: {
        neovimWithNix = super.neovim.override {
          configure = {
            customRC = ''
              set hidden
              set termguicolors
              colorscheme base16-classic-light
              set ignorecase
              set smartcase
            '';
            packages.myVimPackages = with super.vimPlugins; {
              start = [ vim-nix fugitive base16-vim ale ];
              opt = [];
            };
          };
        };
      }
      )
      (self: super: {
	grub2 = super.grub2_full.override { efiSupport = true; };
       })
#      (self: super: {
#        st = super.st.override {
#          patches = [
#            ./st-themed_cursor-0.8.2.diff
#            ];
#          extraLibs = [
#            super.xorg.libXcursor
#            ];
#        };
#      }
#      )
    ];
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  hardware = {
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      support32Bit = true;
      package = pkgs.pulseaudioFull;
    };
    bluetooth.enable = true;
    cpu.amd.updateMicrocode = true;
    u2f.enable = true;
  };

  services = {
    teamviewer.enable = true;
    "nix-serve" = {
      enable = true;
      secretKeyFile = "/etc/nixos/monster.sec";
    };

    lorri.enable = true;
    fwupd.enable = true;
    blueman.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.hplipWithPlugin ];
    };

    nscd.enable = false;

    openssh = {
      enable = true;
      forwardX11 = true;
      useDns = false;
    };

    pcscd.enable = true;

    xserver = {
      videoDrivers = [ "amdgpu" "vesa" ];
      libinput = {
        enable = true;
        naturalScrolling = true;
      };
      enable = true;
      displayManager = {
        lightdm = {
          enable = true;
        };
      };
      layout = "de";
      xkbVariant = "neo";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
      displayManager.defaultSession = "none+xmonad";
    };

    udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
    avahi.enable = true;

#     chrony = {
#       enable = true;
#       servers = ["0.de.pool.ntp.org" "1.de.pool.ntp.org" ];
#     };
    ntp.enable = true; # will be handled by chrony

    #some gnome stuff
    #gvfs.enable = true;
    haveged.enable = true;
    udisks2.enable = true;

    gnome3."gnome-keyring".enable = true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "nfs" "nfs4" ];
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
      };
    };
    enableContainers = true;
    initrd.luks.devices.crypted.device ="/dev/disk/by-uuid/01e5ca58-e145-4585-aa26-255699b7bf81";
  };

  networking = {
    firewall.enable = false;
    hostName = "monster"; # Define your hostname.
    networkmanager = {
      enable = true;
      unmanaged = [ "interface-name:br*" "interface-name:v*" "docker0" "interface-name:kvm*" ];
    };
    resolvconf = {
      dnsSingleRequest = true;
      extraOptions = ["single-request-reopen"];
    };
  };

  console.keyMap = "neo";
  console.earlySetup = true;
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment = {
    pathsToLink = [ "/share/nix-direnv" ];
    shells = [ pkgs.zsh pkgs.bash ];
    systemPackages = with pkgs; [
      nix-direnv
      direnv
      nginx
      manpages
      nfs-utils
      networkmanager
      networkmanager-openvpn
      networkmanagerapplet
      nix-index
      wget
      procps-ng
      mtpfs
      ntfs3g
      nix
      gitFull
      keepassxc
      openvpn
      neovimWithNix
      (haskellPackages.ghcWithPackages(p: [p.xmonad p.xmonad-contrib p.xmonad-extras p.shake p.xmobar]))
      trayer
      st
      tmux
      xorg.xev
      firefox
      mkpasswd
      efibootmgr
      zsh
      bridge-utils
      pciutils
      lshw
      ripgrep
      fwupd
      openssl
      bc
      dmidecode
      iftop
      openssh
      nix
    ];
  };

  fonts = {
    fonts = [
      pkgs.roboto
      pkgs.roboto-mono
      pkgs.dejavu_fonts
      pkgs.iosevka
    ];
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    java = {
      enable = true;
      package = pkgs.jetbrains.jdk;
    };
    bash.enableCompletion = true;
    dconf.enable = true;
    iftop.enable = true;
    iotop.enable = true;
    usbtop.enable = true;
    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };

    ssh = {
      package = pkgs.openssh;
      forwardX11 = true;
      setXAuthLocation = true;
    };

    zsh = {
      enable = true;
      # autosuggestions.enable =  true;
      enableCompletion = true;
      syntaxHighlighting = {
        enable = true;
        highlighters = [ "main" "brackets" "pattern" ];
      };
      ohMyZsh = {
        enable = true;
        plugins = [ "man" "cp" "dirpersist" "pass" "web-search" "cabal" "git" "z" "sudo" ];
        theme = "ys";
      };
    };

  };


  users = {
    mutableUsers = false;
    groups.typetetris.gid = 666;
    users = { 
      root.hashedPassword = "";
      typetetris = {
        isNormalUser = true;
        uid = 666;
        home = "/home/typetetris";
        extraGroups = [
          "wheel"
          "video"
          "audio"
          "disk"
          "networkmanager"
          "docker"
          "libvirtd"
          "kvm"
          "wireshark"
          "dialout"
          "camera"
          "typetetris"
        ];
        hashedPassword = "";
        shell = pkgs.zsh;
      };
    };
  };

  nix.trustedUsers = [ "root" "@wheel" ];
  nix.useSandbox = true;
  nix.binaryCaches = [ 
    "https://cache.nixos.org"
    "https://all-hies.cachix.org"
    "https://haskell-miso.cachix.org"
    "https://arm.cachix.org"
    "https://iohk.cachix.org"
    ];
  nix.trustedBinaryCaches = [ 
    "http://goddess:5000"
    ];
  nix.binaryCachePublicKeys = [
    "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    "haskell-miso.cachix.org-1:JU8k0o/s0G/LtD43BTkrIuLX8NfKktgq7MkgrCdtG6o="
    "arm.cachix.org-1:fGqEJIhp5zM7hxe/Dzt9l9Ene9SY27PUyx3hT9Vvei0="
    "goddess:Rum+QUIGeSCLt+XmlSo/tDRJ1zN2lN8vkjFSl01X5n8="
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    ];

  nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

  security.pam.yubico = {
  	enable = true;
	mode = "challenge-response";
	control = "sufficient";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

   systemd = {
     services = {
       myBridges = {
         enable = true;
         wantedBy = [ "multi-user.target" ];
         script = ''
           ${pkgs.bridge_utils}/bin/brctl addbr br0
           ${pkgs.bridge_utils}/bin/brctl addbr kvm_local
           ${pkgs.bridge_utils}/bin/brctl addbr kvm_prod
           ${pkgs.bridge_utils}/bin/brctl addbr kvm_mgmt
           '';
         serviceConfig = {
           Type = "oneshot";
           RemainAfterExit = true;
         };
       };
     };
#  Needs system-networkd instead of networkmanager
#     network = {
#       netdevs =
#         let
#           entry = elem: {
#             "${elem}" = {
#               enable = true;
#               netdevConfig = {
#                 Kind = "bridge";
#                 Name = elem;
#               };
#             };
#           };
#           bridgeSet = xs: builtins.foldl' (acc: elem: acc // (entry elem)) {} xs;
#         in
#         bridgeSet [ "br0" "kvm_local" "kvm_prod" "kvm_mgmt" ];
#       };
     };

  virtualisation = {
    docker = {
      enable = true;
      autoPrune.enable = true;
    };
    libvirtd = {
      enable = true;
      allowedBridges = [
        "virbr0"
        "kvm_local"
        "kvm_prod"
        "kvm_mgmt"
        "br0"
      ];
    };
    lxc.enable = true;
    virtualbox.host.enable = false; # true; # TODO build error
  };

  security = {
    pki = {
      certificates = [
        (import ./lbm-certificate.nix)
        ];
    };
  };
}
