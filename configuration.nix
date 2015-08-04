{ config, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "bcache" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.kernelPackages = pkgs.linuxPackages_4_1;

  fileSystems."/" =
    { device = "/dev/bcache0";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/74EA-D753";
      fsType = "vfat";
    };

  boot.initrd.luks.devices = [ { name = "cryptroot"; device = "/dev/sda2"; } ];

  swapDevices = [ ];

  nix.maxJobs = 8;

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "qbTop";
  networking.wireless.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Warsaw";

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    wget curl emacs git firefox compton dmenu silver-searcher p7zip
    powertop thunderbird
  ];

  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];
    vaapiDrivers = [ pkgs.vaapiIntel ];
    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
		};
    desktopManager.xterm.enable = false;
    displayManager = {
      auto = {
        enable = true;
        user = "qb";
      };
      desktopManagerHandlesLidAndPower = false;
    };
    deviceSection = ''
      Option "AccelMethod" "sna"
      Option "TearFree" "true"
    '';
    layout = "us";
    xkbOptions = "eurosign:e";
    synaptics = {
      enable = true;
      minSpeed = "1";
      maxSpeed = "2";
      accelFactor = "0.01";
      twoFingerScroll = true;
      palmDetect = true;
      fingersMap = [1 3 2];
      additionalOptions = ''
        Option "HorizHysteresis" "30"
        Option "VertHysteresis" "30"
        Option "FingerLow" "40"
        Option "FingerHigh" "50"
        Option "CoastingFriction" "100"
      '';
    };
  };

  users.extraUsers.qb = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ inconsolata source-code-pro ];
  };

  hardware.enableAllFirmware = true;

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    s3tcSupport = true;
  };

  hardware.bumblebee.enable = true;

  hardware.pulseaudio.enable = true;

  programs.bash.enableCompletion = true;
}
