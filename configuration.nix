# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
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
      support32Bit = true;
      package = pkgs.pulseaudioFull;
    };
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    u2f.enable = true;
  };

  services.pcscd.enable = true;

  # Use a more modern kernel, should have drivers for RX 5700 XT
  boot.kernelPackages = pkgs.linuxPackages_5_4;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices.crypted.device ="/dev/disk/by-uuid/01e5ca58-e145-4585-aa26-255699b7bf81";

  networking.hostName = "monster"; # Define your hostname.
  networking.networkmanager.enable = true;

  i18n = {
    consoleKeyMap = "neo";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    nix-index
    wget
    roboto
    roboto-mono
    procps-ng
    mtpfs
    ntfs3g
    nix
    keepassxc
    openvpn
    neovim
    (haskellPackages.ghcWithPackages(p: [p.xmonad p.xmonad-contrib p.xmonad-extras p.shake p.xmobar]))
    trayer
    st
    tmux
    xorg.xev
  ];

  programs.bash.enableCompletion = true;

  services.openssh.enable = true;

  services.printing.enable = true;

  services.xserver = {
    videoDrivers = [ "amdgpu" "vesa" ];
    enable = true;
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
    windowManager.default = "xmonad";
  };

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];

  users.mutableUsers = false;
  users.extraGroups.typetetris.gid = 666;
  users.extraUsers.typetetris = {
    isNormalUser = true;
    uid = 666;
    home = "/home/typetetris";
    extraGroups = [ "wheel" "typetetris" "networkmanager" "docker" ];
#    hashedPassword = 
  };

  nix.trustedUsers = [ "root" "@wheel" ];
  nix.useSandbox = true;
  nix.binaryCaches = [ 
    "https://cache.nixos.org"
    "https://all-hies.cachix.org"
    "https://haskell-miso.cachix.org"
    "https://arm.cachix.org"
    ];
  nix.trustedBinaryCaches = [ 
    "http://goddess:8080"
    ];
  nix.binaryCachePublicKeys = [
    "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    "haskell-miso.cachix.org-1:JU8k0o/s0G/LtD43BTkrIuLX8NfKktgq7MkgrCdtG6o="
    "arm.cachix.org-1:fGqEJIhp5zM7hxe/Dzt9l9Ene9SY27PUyx3hT9Vvei0="
    "goddess:Rum+QUIGeSCLt+XmlSo/tDRJ1zN2lN8vkjFSl01X5n8="
    ];
  virtualisation.docker.enable = false;

  security.sudo = {
    enable = true;
  };

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
}
