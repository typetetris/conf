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
              colorscheme base16-classic-dark
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
    ];
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  hardware = {
    nvidia.prime = {
      sync.enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    enableRedistributableFirmware = true;
   #opengl = {
   #  enable = true;
   #  driSupport = true;
   #  driSupport32Bit = true;
   #  extraPackages = [
   #  ];
   #};
    pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      support32Bit = true;
      package = pkgs.pulseaudioFull;
    };
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  services = {
    "nix-serve" = {
      enable = true;
      secretKeyFile = "/etc/nixos/wolferic-laptop.sec";
    };

    fwupd.enable = true;
    blueman.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.hplipWithPlugin ];
    };

    openssh = {
      enable = true;
      forwardX11 = true;
      useDns = false;
    };

    pcscd.enable = true;

    xserver = {
      videoDrivers = [ "displaylink" "modesetting" ];
      libinput = {
        enable = true;
        touchpad.naturalScrolling = true;
      };
      layout = "de";
      xkbVariant = "neo";
      
      enable = true;
      displayManager = {
        lightdm = {
          enable = true;
        };
      };
      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.xmonad-contrib
            haskellPackages.xmonad-extras
            haskellPackages.xmonad
          ];
        };
      };
      displayManager.defaultSession = "xfce+xmonad";
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
    hardware.bolt.enable = true;
  };

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/294cbc19-89b0-467d-b42d-73b7f7cd3c67";
      allowDiscards = true;
      preLVM = true;
    };
    enableContainers = true;
  };

  networking = {
    firewall.enable = false;
    hostName = "wolferic-laptop"; # Define your hostname.
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
      xscreensaver
      wireguard-tools
      wireguard
      nvme-cli
      nix-direnv
      direnv
      nginx
      manpages
      nfs-utils
      networkmanager
      networkmanager-openvpn
      networkmanager-fortisslvpn
      networkmanagerapplet
      nix-index
      wget
      procps-ng
      mtpfs
      ntfs3g
      gitFull
      keepassxc
      openvpn
      neovimWithNix
      (haskellPackages.ghcWithPackages(p: [p.xmonad p.xmonad-contrib p.xmonad-extras p.shake p.xmobar]))
      trayer
      st
      jq
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
      (import ./nixops.nix)
      xfce.xfce4-panel
      virt-manager
    ];
  };

  fonts = {
    fonts = [
      pkgs.roboto
      pkgs.roboto-mono
      pkgs.dejavu_fonts
      pkgs.iosevka
      pkgs.nerdfonts
    ];
  };

  programs = {
    mosh.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    java = {
      enable = true;
#      package = pkgs.jetbrains.jdk;
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
    users = { 
      root.hashedPassword = "$6$ei9AhNge$wp7lm1Oqy1OPgFtnB68qNShnks500RjNit8f4HQDDuItzh2YRGFWatFX0Rr3LKG396vnD3LvwxPpUFxlbGLh1/";
      wolferic = {
        isNormalUser = true;
        uid = 10000;
        home = "/home/wolferic";
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
        ];
        hashedPassword = "$6$ei9AhNge$wp7lm1Oqy1OPgFtnB68qNShnks500RjNit8f4HQDDuItzh2YRGFWatFX0Rr3LKG396vnD3LvwxPpUFxlbGLh1/";
        shell = pkgs.zsh;
      };
    };
  };

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    useSandbox = true;
    binaryCaches = [ 
        "https://cache.nixos.org"
        "https://hydra.iohk.io"
      ];
      trustedBinaryCaches = [ 
        "http://goddess:5000"
        "http://tilia.lbaum.eu:8081"
        "http://gitlab-runner-03.lbaum.eu:5000"
        "http://gitlab-runner-04.lbaum.eu:5000"
        "http://dirac.lbaum.eu:5000"
      ];
      binaryCachePublicKeys = [
        "nixos-gitlab-runner:9bFPXTSEb8D8RNOeqG9+dq5wMGLisDsw0ubx1f7B5fU="
        "haskell-miso.cachix.org-1:JU8k0o/s0G/LtD43BTkrIuLX8NfKktgq7MkgrCdtG6o="
        "arm.cachix.org-1:fGqEJIhp5zM7hxe/Dzt9l9Ene9SY27PUyx3hT9Vvei0="
        "goddess:Rum+QUIGeSCLt+XmlSo/tDRJ1zN2lN8vkjFSl01X5n8="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "tilia.lbaum.eu:Sk/TixGlwREoBgmfXQnQjYhGnxzrDfFobu664eutZJI="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "dirac:wPsHcPovMg/v/67TR/wWpRVRmZ1OqO2NoUud/gC5I2k="
      ];

      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
      '';

    };

   systemd = {
     services = {
       ntpd = {
         serviceConfig = {
           TimeoutStopSec = 10;
         };
       };
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
        (builtins.readFile ./forti.crt)
        ];
    };
  };

  system.stateVersion = "20.09";
}

