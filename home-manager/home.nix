{ config, pkgs, inputs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "bano";
  home.homeDirectory = "/home/bano";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # Allow all unfree cus why the heck not
  nixpkgs.config.allowUnfree = true;

  # In order to be a special little snowflake...
  nix = {
    package = pkgs.nix;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    # Epic bragging rights
    neofetch

    # My ThinkPad can only handle so much cord bloat... Also themes and plugins!
    armcord

    # Never have too many mods
    prismlauncher

    # Better than Discord calls
    teamspeak_client

    # Funny image sorcery
    imagemagick

    # Apparantly TF2 is on this thing...
    steam

    # Better safe than sorry
    clamav

    # Honestly cannot believe this whack is on here
    craftos-pc

    # Some funky stuff from charm.sh
    gum
    glow
    soft-serve

    # MFW SC3 always ropes me back into modelling
    blockbench-electron

    # Some non-windoos thing to read docs and stuff
    abiword

    # Science can be fun
    the-powder-toy

    # Maximum privacy, cus ig that's the linux way
    tor

    # Ultimate funny of funnies
    neo-cowsay

    # OpenGL debugging
    glxinfo

    # The frontend in me needs this
    eyedropper

    # Better Gimp
    krita

    # Mmmmm rainbows
    lolcat

    # Goodbye, rm!
    trashy

    # Required for GPG
    pinentry-qt

    # CHOO-CHOOOOOO!!
    sl

    # Password management
    bitwarden
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/bano/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Alias shenanigans
  programs.bash.enable = true;
  programs.bash.shellAliases = {
    neofetch="neofetch --ascii /home/bano/Pictures/ASCII/NixOwOs.txt --ascii_colors 4 5";
  };

  # Rando numbers go brrr
  programs.htop.enable = true;

  # neovim, because neovim
  programs.neovim.enable = true;

  # Git.
  programs.git.enable = true;
  programs.git.userName = "LDexter";
  programs.git.userEmail = "ldextermiller@gmail.com";
  programs.git.signing.signByDefault = true;
  programs.git.signing.key = builtins.readFile "${inputs.self}/${config.home.username}/key.txt";

  # To look all official
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;
  services.gpg-agent.extraConfig = ''
    pinentry-program ${pkgs.pinentry.qt}/bin/pinentry
  '';

  # is cool
  programs.lf.enable = true;

  # Actually sane terminal emulator
  programs.kitty.enable = true;
  # Major config
  programs.kitty.shellIntegration.mode = "enabled no-cursor";
  programs.kitty.theme = "Sakura Night";
  # General settings
  programs.kitty.settings = {
    cursor_shape = "block";
    background_opacity = "0.95";
  };

  # Get outta here with your Firefox
  programs.qutebrowser.enable = true;
  # General settings
  programs.qutebrowser.searchEngines = {
    w = "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1";
    aw = "https://wiki.archlinux.org/?search={}";
    nw = "https://nixos.wiki/index.php?search={}";
    np = "https://search.nixos.org/packages?channel=23.05&from=0&size=50&sort=relevance&type=packages&query={}";
    ya = "https://yandex.com/images/search?text={}";
    yt = "https://www.youtube.com/results?search_query={}";
  };
  programs.qutebrowser.settings = {
    # Primary Light:     #FF5CE1
    # Primary Dark:      #B55088
    # Secondary Light:   #007F7F
    # Secondary Dark:    #0F6365
    # Tertiary:          #181426
    # Success:           #34703E
    # Warning:           #FEAE34
    # Error:             #E43B44
    colors = {
      # Link hint colours
      hints = {
        bg = "#FF5CE1";
        fg = "#FFFFFF";
      };
      # Completion colors
      completion.item.selected.bg = "#FF5CE1";
      completion.item.selected.border.bottom = "#FF5CE1";
      completion.item.selected.border.top = "#FF5CE1";
      # Message colours
      messages.error.bg = "#E43B44";
      messages.warning.bg = "#FEAE34";
      # Tab colours
      tabs.bar.bg = "#FF5CE1";
      tabs.even.bg = "#007F7F";
      tabs.odd.bg = "#0F6365";
      tabs.selected.even.bg = "#FF5CE1";
      tabs.selected.odd.bg = "#FF5CE1";
      # Status colours
      statusbar.normal.fg = "#FF5CE1";
      statusbar.url.fg = "#007F7F";
      statusbar.url.success.https.fg = "#34703E";
      statusbar.url.error.fg = "#E43B44";
      statusbar.url.warn.fg = "#FEAE34";
      # Dark reader
      webpage.darkmode.enabled = true;
    };
  };

  # Badass music player
  programs.ncmpcpp.enable = true;

  # Multiplexing is kinda nifty
  programs.tmux.enable = true;

  # Best software in existence
  programs.thefuck.enable = true;
}
