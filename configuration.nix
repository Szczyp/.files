{ config, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/715d3885-c1c8-4307-a0a4-5631fa7c0ac0";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/8E65-6E4C";
    fsType = "vfat";
  };

  fileSystems."/storage" = {
    device = "/dev/disk/by-uuid/40a06ed3-41a5-484d-83e8-9e8a2c8f3c8a";
    fsType = "ext4";
  };

  swapDevices = [ ];

  nix.maxJobs = 8;

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_4_1;

  security.sudo.wheelNeedsPassword = false;

  networking = {
    hostName = "server";
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ 445 139 631 8200 9091 51413];
      allowedUDPPorts = [ 137 138 1900 ];
    };
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget git emacs mcron rdiff-backup screen smartmontools ntfs3g
  ];

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
  };

  services.printing = {
    enable = true;
    listenAddresses = [ "192.168.1.2:631" "server:631" ];
    drivers = [ pkgs.hplip ];
    extraConf = ''
      DefaultEncryption Never
      <Location />
        Order allow,deny
        Allow from 192.168.1.*
      </Location>
      <Location /admin>
        Order allow,deny
        Allow from 192.168.1.*
      </Location>
      <Location /admin/conf>
        Order allow,deny
        Allow from 192.168.1.*
      </Location>
    '';
  };

  services.samba = {
    enable = true;
    nsswins = true;
    shares = {
      public = {
        path = "/storage";
        public = "yes";
        "only guest" = "yes";
        available = "yes";
        browsable = "yes";
        writable = "yes";
      };
    };
    extraConfig = ''
      workgroup = DOM
      server string = server
      hosts allow = 192.168.1. 127.
      guest account = guest
      map to guest = Bad User
      wins support = yes
      wins proxy = yes
      dns proxy = yes
    '';
  };

  services.minidlna = {
    enable = true;
    mediaDirs = [ "V,/storage/MOVIES" "P,/storage/PHOTOS" "A,/storage/MUSIC" ];
  };

  services.transmission = {
    enable = true;
    settings = {
      download-dir = "/storage/Downloads";
      incomplete-dir = "/storage/Downloads/part";
      incomplete-dir-enabled = true;
      rpc-bind-address = "192.168.1.2";
      rpc-whitelist = "127.0.0.1,192.168.1.*";
      speed-limit-up-enabled = true;
    };
  };

  users.extraUsers.qb = {
    extraGroups = [ "wheel" "transmission" ];
    isNormalUser = true;
  };

  users.extraUsers.guest = {
    uid = 1100;
    description = "Guest User";
    password = "";
    extraGroups = [ "users" "transmission" ];
  };

  programs.bash.enableCompletion = true;
}
