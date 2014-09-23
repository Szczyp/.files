{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_3_16;

  networking.hostName = "nixos";

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget curl emacs git firefox compton dmenu silver-searcher
    leiningen phantomjs
    haskellPackages.ghcMod haskellPackages.hoogle haskellPackages.cabal2nix
  ];

  services.xserver = {
    enable = true;
    windowManager.xmonad = { enable = true;
    			     enableContribAndExtras = true;
			   };
    desktopManager.xterm.enable = false;
    displayManager.slim = { enable = true;
    			    # autoLogin = true;
			    defaultUser = "qb";
			  };
    layout = "us";
    xkbOptions = "eurosign:e";
  };

  users.extraUsers.qb = {
    name = "qb";
    group = "users";
    extraGroups = [ "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal" ];
    uid = 1000;
    createHome = true;
    home = "/home/qb";
    shell = "/run/current-system/sw/bin/bash";
  };

  fonts = {
  	enableFontDir = true;
	fonts = with pkgs; [
	      inconsolata
	];
  };
}
