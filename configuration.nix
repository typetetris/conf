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
      (self: super: {
        st = super.st.override {
          patches = [
            ./st-themed_cursor-0.8.2.diff
            ];
          extraLibs = [
            super.xorg.libXcursor
            ];
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
    sane.enable = true;
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      support32Bit = true;
      package = pkgs.pulseaudioFull;
    };
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    u2f.enable = true;
  };

  services = {

    nginx = {
      enable = true;
      virtualHosts = {
        "dirac.lbaum.eu" = {
          forceSSL = true;
          sslCertificate = "/etc/nixos/certificate.pem";
          sslCertificateKey = "/etc/nixos/private.key";
          locations =
            let
              delegateEndpoint = endp: {
                proxyWebsockets = true;
                proxyPass = "http://localhost:9090/${endp}";
                extraConfig =
                  ''
                    

                    client_max_body_size 0;
                    proxy_intercept_errors on;
                    proxy_redirect default;
                    proxy_set_header Host $host;
                    proxy_set_header Connection "";
                    proxy_set_header Proxy "";
                    proxy_set_header X-Forwarded-Proto $scheme;
                    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  '';
              };
            in
            {
            "/management" = delegateEndpoint "management";
            "/conference" = delegateEndpoint "conference";
            "/demo" = {
                proxyWebsockets = true;
                proxyPass = "http://localhost:1234/";
            };
            "/api" = {
                proxyWebsockets = true;
                proxyPass = "http://localhost:8081/";
            };
          };
        };
      };
    };

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
      videoDrivers = [ "intel" ];
      dpi = 282;
      libinput = {
        enable = true;
        naturalScrolling = true;
      };
      enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          greeters = {
            gtk = {
              cursorTheme = {
                name = "Vanilla-DMZ";
                package = pkgs.vanilla-dmz;
                size = 128;
              };
            };
          };
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

    chrony = {
      enable = true;
      servers = ["0.de.pool.ntp.org" "1.de.pool.ntp.org" ];
    };
    ntp.enable = false; # will be handled by chrony

    #some gnome stuff
    #gvfs.enable = true;
    haveged.enable = true;
    udisks2.enable = true;
  };

  # Use a more modern kernel, should have drivers for RX 5700 XT needs unstable channel ?
  # boot.kernelPackages = pkgs.linuxPackages_5_4;
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        consoleMode = "2";
      };
      grub = {
        enable = false;
        efiSupport = true;
        device = "/dev/nvme0n1";
      };
    };
    initrd = {
      kernelModules = [ "i915" ];
    };
    enableContainers = true;
    #kernelPackages = pkgs.linuxPackages_latest;
  };

  networking.hostName = "dirac"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    unmanaged = [ "interface-name:br*" "interface-name:v*" "docker0" "interface-name:kvm*" ];
  };

  # Select internationalisation properties.
  console.font = "latarcyrheb-sun32";
  console.keyMap = "neo";
  console.earlySetup = true;
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment = {
    shells = [ pkgs.zsh pkgs.bash ];
    systemPackages = with pkgs; [
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
      startAgent = true;
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



  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  # hardware.pulseaudio.enable = true;

  users = {
    mutableUsers = false;
    users = {
      wolferic = {
        isNormalUser = true;
        home = "/home/wolferic";
        description = "Eric Wolf";
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
        hashedPassword = import ./wolferic-password.nix;
        shell = pkgs.zsh;
      };
    };
  };

  # Enable the X11 windowing system.



  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  nix.trustedUsers = [ "root" "@wheel" ];
  nix.useSandbox = true;
  nix.binaryCaches = [ 
    "https://cache.nixos.org"
    "https://all-hies.cachix.org"
    "https://haskell-miso.cachix.org"
    "https://arm.cachix.org"
  ];
  nix.trustedBinaryCaches = [ 
    "http://192.168.100.136:5000"
  ];
  nix.binaryCachePublicKeys = [
    "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    "haskell-miso.cachix.org-1:JU8k0o/s0G/LtD43BTkrIuLX8NfKktgq7MkgrCdtG6o="
    "arm.cachix.org-1:fGqEJIhp5zM7hxe/Dzt9l9Ene9SY27PUyx3hT9Vvei0="
    "svenwega:jwJvgqyMcBmsqy3M01bgaZTxe/gBK+tLoktcqWYYELo="
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?


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

