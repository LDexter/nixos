{ config, pkgs, lib, inputs, ... }:

let
  inherit (builtins) concatStringsSep isBool;
  inherit (lib) mkOption types escapeShellArg mapAttrsToList mapAttrs' nameValuePair;
in

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

  # When some Electron app devs are too stubborn for an update
  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
  ];
  
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
    vesktop

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

    # Honestly cannot believe this whack has a nix package
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

    # Fisce
    asciiquarium

    # For doom-modeline icons
    nerdfonts

    # Web development
    nodejs_20
    sass

    # Outta here with your normie Firefox!
    vieb

    # I need my videos and music local thanks
    yt-dlp
    ytmdl
    ffmpeg-full
    picard

    # A simple video player. Is this too much to ask??
    mplayer
    qmplay2

    # Pulling files
    wget

    # Coolest clock around!
    peaclock
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

    ".vieb/colors/compactCustom.css".source = ./compactCustom.css;
    ".vieb/viebrc".text = concatStringsSep "\n" (
        mapAttrsToList (n: v:
          if isBool v then
            "set ${if v then "" else "no"}${n}"
          else
            "set ${n}=${v}"
        ) {
          adblocker = "update";
          downloadmethod = "confirm";
          suspendbackgroundtab = false;
          tabcycle = false;
          tabreopenposition = "previous";
          dialogconfirm = "show";
          nativetheme = "dark";
          darkreader = true;
          notificationforpermissions = "all";
          useragent = "%default";
          userscript = true;
          vimcommand = ''"emacsclient -c"'';
        } ++ map (x: "unmap ${x}") [
        ] ++ mapAttrsToList (n: v:
          "nmap ${n} ${v}"
        ) {
          # Alternatives for Workman layout while maintaining QWERTY backwards compatibility
          # Movement
          "y" = "<scrollLeft>";        # h alt
          "n" = "<scrollDown>";        # j alt
          "e" = "<scrollUp>";          # k alt
          "o" = "<scrollRight>";       # l alt

          # History and tabs
          "Y" = "<backInHistory>";     # H alt
          "N" = "<nextTab>";           # J alt
          "E" = "<previousTab>";       # K alt
          "O" = "<forwardInHistory>";  # L alt

          # Substitutions
          # The mess of yank
          "cR<Any>" = "<pageRSSLinkToClipboard>";  # yR<Any> sub
          "cRL" = "<pageRSSLinksList>";            # yRL sub
          "ce" = "<pageToClipboardEmacs>";         # ye sub
          "cf" = "<startFollowCopyLink>";          # yf sub
          "ch" = "<pageToClipboardHTML>";          # yh sub
          "cm" = "<pageToClipboardMarkdown>";      # ym sub
          "cr" = "<pageToClipboardRST>";           # yr sub
          "cs" = "<p.copyText>";                   # ys sub
          "ct" = "<pageTitleToClipboard>";         # yt sub
          "cc" = "<pageToClipboard>";              # yy sub
          "c" = "<pageToClipboard>";               # Backup if no key code

          # Everything else
          "." = "<nextSearchMatch>";             # n sub
          "," = "<previousSearchMatch>";         # N sub
          "s" = "<toExploreMode>";               # e sub ("s" for <p.moveToMouse> would suggest I use a mouse)
          "S" = "<:tabnew><toExploreMode>";      # E sub
          "tO" = "<moveTabEnd>";                 # O sub (no "o" default)
          "FS" = "<startFollowNewSplit>";        # S sub
          "FV" = "<startFollowNewVerSplit>";     # V sub
          "p" = "<p.start>";                     # v sub (a little phonetics balance for messing with e)
          "v" = "<openFromClipboard>";           # p sub
          "V" = "<:tabnew><openFromClipboard>";  # P sub
        } ++ mapAttrsToList (n: v: "${n} ${v}")
          {
            "colorscheme" = "compactCustom";
          });
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
  };

  # PROGRAMS
  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    # Alias shenanigans
    bash.enable = true;
    bash.shellAliases = {
      # NixOwOs setup
      neofetch="neofetch --ascii /home/bano/nixos/home-manager/NixOwOs.txt --ascii_colors 4 5";

      # Shutdown and reboot
      sd="shutdown now";
      rb="reboot";
      
      # Switches
      nxsw="sudo nixos-rebuild --flake ~/nixos switch";
      hmsw="home-manager switch --flake ~/nixos/home-manager";
      
      # Updates
      nxup="nix flake update ~/nixos";
      hmup="nix flake update ~/nixos/home-manager";

      # Emacs
      estart="systemctl --user start emacs.service";
      eopen="emacsclient --create-frame";
      ereload="systemctl --user daemon-reload";
      ekill="emacsclient -e '(kill-emacs)'";
      epkgs="nix-env -f '<nixpkgs>' -qaP -A emacsPackages";

      # LTE networking
      ltestart="sudo nmcli c add type gsm ifname '*' con-name NixPad-CON apn cdc-wdm0";
      lteon="nmcli r wwan on";
      lteoff="nmcli r wwan off";
    };

    # Rando numbers go brrr
    htop.enable = true;
    
    # The lifestyle
    emacs = {
      enable = true;
      extraPackages = epkgs: 
        [
          # General packages
          epkgs.use-package
          epkgs.doom-themes
          epkgs.ef-themes
          epkgs.magit
          epkgs.melpa-upstream-visit
          epkgs.org
          epkgs.command-log-mode
          epkgs.evil
          epkgs.evil-collection
          epkgs.ivy
          epkgs.ivy-rich
          epkgs.counsel
          epkgs.doom-modeline
          epkgs.nerd-icons
          epkgs.nerd-icons-completion
          epkgs.nerd-icons-dired
          epkgs.nerd-icons-ibuffer
          epkgs.nerd-icons-ivy-rich
          epkgs.rainbow-delimiters
          epkgs.which-key
          epkgs.helpful
          epkgs.general
          epkgs.hydra
          epkgs.yasnippet

          # Language packages
          epkgs.nix-mode

          # Web development
          epkgs.react-snippets
          epkgs.rjsx-mode
          epkgs.json-mode
          epkgs.scss-mode
        ];
      extraConfig = builtins.readFile(./init.el);
    };

    # Git.
    git = {
      enable = true;
      userName = "LDexter";
      userEmail = "ldextermiller@gmail.com";
      # Dual GPG key attempt
      #signing.signByDefault = true;
      #git.signing.key = "2839 D41D DD34 6506 8E12  9A37 D2A5 0AFC 02B7 9AA7";
      #builtins.readFile "${inputs.self}/${config.home.username}/key.txt";
    };

    # To look all official
    gpg.enable = true;

    # is cool
    lf.enable = true;

    # Actually sane terminal emulator
    kitty = {
      enable = true;
      # Major config
      shellIntegration.mode = "enabled no-cursor";
      theme = "Sakura Night";
      font.package = pkgs.dejavu_fonts;
      font.name = "DejaVu Sans";
      font.size = 13;
      # General settings
      settings = {
        cursor_shape = "block";
        background_opacity = "0.90";
      };
    };

    # Badass music player
    ncmpcpp = {
      enable = true;
      mpdMusicDir = "~/Music";
      package = pkgs.ncmpcpp.override { visualizerSupport = true; };

      settings = {
        visualizer_data_source = "/tmp/mpd.fifo";
        visualizer_output_name = "my_fifo";
        visualizer_in_stereo = "yes";
        visualizer_type = "wave_filled";
        visualizer_look = "<#";
        visualizer_color = "220, 213, 206, 199, 162, 125";

        alternative_header_first_line_format = "$b$(54)$aqqu$/a$9 $3{%t}|{%f} $9$(54)$atqq$/a$9$/b";
        alternative_header_second_line_format = "{{$4$b%a$/b$9}{ - $(106)%b$9}{ ($4%y$9)}}|{%D}";
        
        main_window_color = 14;
        alternative_ui_separator_color = 54;
        window_border_color = 54;
        progressbar_color = 185;
        song_columns_list_format = "(20)[14]{a} (45)[3]{t|f:Title} (25)[106]{b} (7f)[220]{l}";

        progressbar_look = "─🌸·";
        playlist_disable_highlight_delay = 0;
        user_interface = "alternative";
        lyrics_fetchers = "azlyrics, genius, musixmatch, sing365, metrolyrics, justsomelyrics, jahlyrics, plyrics, tekstowo, zeneszoveg, internet";
        follow_now_playing_lyrics = "yes";
        fetch_lyrics_for_current_song_in_background = "yes";

        startup_screen = "visualizer";
        startup_slave_screen = "playlist";
        startup_slave_screen_focus = "yes";
        locked_screen_width_part = 60;

        jump_to_now_playing_song_at_start = "yes";
        display_bitrate = "yes";
        mouse_support = "no";
      };
    };

    # Sicksass visualisation
    cava.enable = true;

    # Multiplexing is kinda nifty
    tmux.enable = true;

    # Best software in existence
    thefuck.enable = true;

    # Simple image viewing!
    feh.enable = true;

    # Multi-display configuration
    # Disabled until needs arise
    #autorandr.enable = true;
  };
  
  # Ensuring gpg has access to pinentry
  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

  # For running Emacs as daemon
  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
  };

  services.mpd-discord-rpc = {
    enable = true;
    settings = {
      format = {
        details = "Weebing to $title";
        state = "By $artist, from $album";
      };
    };
  };
  
  # autorandr systemd service
  # Disabled until needs arise
  #services.autorandr.enable = true;
}
