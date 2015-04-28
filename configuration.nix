{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_3_18;

  networking.hostName = "nixos";

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget curl emacs git firefox compton dmenu silver-searcher 
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
			    theme = pkgs.fetchurl {
			      url = "https://github.com/jagajaga/nixos-slim-theme/archive/Final.tar.gz";
			      sha256 = "4cab5987a7f1ad3cc463780d9f1ee3fbf43603105e6a6e538e4c2147bde3ee6b";
			    };
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
    fonts = with pkgs; [ inconsolata source-code-pro ];
  };
}
