{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_4_0;

  networking.hostName = "nixos";

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget curl emacs git firefox compton dmenu silver-searcher p7zip
  ];

  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
		};
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    displayManager.auto.enable = true;
    displayManager.auto.user = "qb";
    layout = "us";
    xkbOptions = "eurosign:e";
  };

  users.extraUsers.qb = {
    name = "qb";
    group = "users";
    extraGroups = [ "wheel" ];
    uid = 1000;
    createHome = true;
    home = "/home/qb";
    shell = "/run/current-system/sw/bin/zsh";
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ inconsolata source-code-pro ];
  };

  programs.zsh.enable = true;
}
