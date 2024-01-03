# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  networking.hostName = "NixPad"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  networking.nat.enable = true;
  networking.nat.internalInterfaces = [
    "eth0"
  ];
  systemd.services.ModemManager =  {
    wantedBy = [ "networking.target" ];
    scriptArgs = "--debug";
  };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_AU.UTF-8";
    LC_IDENTIFICATION = "en_AU.UTF-8";
    LC_MEASUREMENT = "en_AU.UTF-8";
    LC_MONETARY = "en_AU.UTF-8";
    LC_NAME = "en_AU.UTF-8";
    LC_NUMERIC = "en_AU.UTF-8";
    LC_PAPER = "en_AU.UTF-8";
    LC_TELEPHONE = "en_AU.UTF-8";
    LC_TIME = "en_AU.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # ENABLE BLUETOOTH, UNIQUE TO LAPTOP INSTANCE
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # Blueman for easy setup
  services.blueman.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.bano = {
    isNormalUser = true;
    description = "bano";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      # Simple text editor, will remove eventually
      kate

      # Driver support for Vulkan
      mesa

      # Display and login management
      libsForQt5.sddm
      libsForQt5.sddm-kcm
      libsForQt5.kcmutils

      # LTE Setup
      networkmanager
      modemmanager
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget

  ];


  ## Nix settings INPUTS AND OTHER STUFF DISABLED

  # Pin nixpkgs and add "self" to the flake registry
  #nix.registry = {
  #  nixpkgs.flake = inputs.nixpkgs;
  #  self.flake = inputs.self;
  #};

  # Set $NIX_PATH to find anything under "/etc/nix/path"
  #nix.nixPath = ["/etc/nix/path"];

  # Link nixpkgs and "self" so they can will be added to $NIX_PATH
  #environment.etc = {
  #  "nix/path/nixpkgs".source = inputs.nixpkgs;
  #  "nix/path/self".source = inputs.self;
  #};
  

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?


  # Drivers
  hardware.opengl.enable = true;
  boot.kernelParams = [ "i915.force_probe=0a16" ];
  hardware.opengl.extraPackages = [ pkgs.mesa.drivers ];


  # WIREGUARD SETUP
  networking.wg-quick.interfaces = {
    wg0 = {
      autostart = false;
      address = [ "10.66.66.5/32" "fd42:42:42::5/128" ];
      dns = [ "1.1.1.1" "1.0.0.1" ];
      privateKeyFile = "/home/bano/wg/.wgprivkey";

      peers = [{
        publicKey = "GzIe5T+UPu6rg6PV/hCY1EycppeTSlhJHgQBEhVGjDo=";
        presharedKeyFile = "/home/bano/wg/.wgpresharedkey";
        allowedIPs = [ "0.0.0.0/0" "::/0" ];
        endpoint = "46.29.236.25:51820";
      }];
    };
  };
}
