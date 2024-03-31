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

  # Ensuring cursor theme loads in Hyprland
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.catppuccin-cursors.lattePink;
    name = "Catppuccin-Latte-Pink-Cursors";
    size = 32;
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
    #the-powder-toy # Breaking changes on 18/1/24 update

    # Maximum privacy, cus ig that's the linux way
    tor

    # Ultimate funny of funnies
    charasay

    # OpenGL debugging
    glxinfo

    # The frontend in me needs this
    eyedropper

    # Open source Adobe, ig
    krita
    inkscape

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
    nodePackages.pnpm
    sass

    # Outta here with your normie Firefox!
    vieb

    # MFW compatibility :(
    firefox

    # I need my videos and music local thanks
    yt-dlp
    #ytmdl - Broken on 12/1/2024 with distutils.errors.DistutilsError
    ffmpeg-full
    picard

    # A simple video player. Is this too much to ask??
    mplayer
    qmplay2

    # Pulling files
    wget

    # Coolest clock around!
    peaclock

    # CLI for the Quantum Mechanical Keyboard firmware
    qmk
    gnumake
    
    # Rust
    rustup
    cargo-generate
    trunk
    gcc

    # "fastest text uwuifier in the west"
    uwuify

    # "Professional Editing, Color, Effects and Audio Post!"
    davinci-resolve

    # Make do until Davinci GPU issues fixed
    libsForQt5.kdenlive

    # Quick and easy audio
    audacity
    
    # Hyprland utilities
    grim
    slurp
    wl-clipboard
    hyprpicker
    wireplumber
    libsForQt5.qt5.qtwayland
    qt6.qtwayland
    qt5ct
    libva
    inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
    inputs.hyprland-contrib.packages.${pkgs.system}.shellevents
    inputs.hyprland-contrib.packages.${pkgs.system}.try_swap_workspace
    inputs.hyprland-contrib.packages.${pkgs.system}.scratchpad
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
    ".vieb/userscript/".source = ./vieb/userscript;
    ".vieb/userstyle/".source = ./vieb/userstyle;
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
          darkreaderblocklist = builtins.toJSON
            [
              "piguman3"
              "cerakey"
              "rust-lang"
            ];
          notificationforpermissions = "blocked";
          permissionhid = "allow";
          permissionsallowed = builtins.toJSON
            [
              ".*chat.openai\.com.*~mediadevices"
            ];
          useragent = "%default";
          userscript = true;
          userstyle = true;
          vimcommand = ''"emacsclient -c"'';
          favoritepages = builtins.toJSON
            [
              "graincult.com"
              "youtube.com"
              "myanimelist.net"
              "chat.openai.com"
              "wiki.hyprland.org"
              "github.com/Alexays/Waybar/wiki"
              "github.com/LDexter/nixos"
              "https://doc.rust-lang.org/book"
              "ratatui.rs"
              "nixos.org/manual/nix/stable/language"
              "nix-community.github.io/home-manager/options.xhtml"
              "mail.google.com"
              "amazon.com.au"
              "codecademy.com"
            ];
          
          searchwords = builtins.toJSON
            {
              # Standard search
              w = "en.wikipedia.org/wiki/Special:Search?search=%s&go=Go&ns0=1";
              aw = "wiki.archlinux.org/?search=%s";
              nw = "nixos.wiki/index.php?search=%s";
              nr = "nixos.org/manual/nix/stable/language/?search=%s";
              np = "search.nixos.org/packages?channel=23.11&from=0&size=50&sort=relevance&type=packages&query=%s";
              gh = "github.com/search?q=%s";
              ghu = "github.com/search?q=%s&type=users";
              yt = "youtube.com/results?search_query=%s";
              ya = "yandex.com/images/search?text=%s";
              az = "amazon.com.au/s?k=%s";
              tp = "thinkwiki.org/w/index.php?search=%s&title=Special%3ASearch&go=Go";
              mal = "myanimelist.net/search/all?q=%s&cat=all";
              imdb = "imdb.com/find?q=%s";

              # Development docs
              rs = "doc.rust-lang.org/book/?search=%s";
              rat = "docs.rs/ratatui/latest/ratatui/?search=%s";
              
              # Japanese/English
              jpen = "deepl.com/translator#ja/en/%s";
              enjp = "deepl.com/translator#en/ja/%s";
              # Russian/English
              ruen = "deepl.com/translator#ru/en/%s";
              enru = "deepl.com/translator#en/ru/%s";
            };
          
        } ++ map (x: "unmap ${x}") [
        ] ++ mapAttrsToList (n: v:
          # Normal Mode
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

          # Everything else
          "." = "<nextSearchMatch>";               # n sub
          "," = "<previousSearchMatch>";           # N sub
          "s" = "<toExploreMode>";                 # e sub ("s" for <p.moveToMouse> would suggest I use a mouse)
          "S" = "<:tabnew><toExploreMode>";        # E sub
          "mo" = "<moveTabEnd>";                   # O sub (no "o" default)
          "my" = "<moveTabStart>";                 # I sub
          "mn" = "<moveTabForward>";               # In line with mo
          "me" = "<moveTabBackward>";              # In line with my
          "FS" = "<startFollowNewSplit>";          # S sub
          "FV" = "<startFollowNewVerSplit>";       # V sub
          "FT" = "<startFollowNewTab>";            # F sub
          "p" = "<p.start>";                       # v sub (a little phonetics balance for messing with e)
          "v" = "<openFromClipboard>";             # p sub
          "V" = "<:tabnew><openFromClipboard>";    # P sub
          "I" = "<toInsertMode>";                       # Insert while capslock
        } ++ mapAttrsToList (n: v:
          # Pointer Mode
          "pmap ${n} ${v}"
        ) {
          # Workman alternatives
          # Movement
          "y" = "<p.moveLeft>";        # h alt
          "n" = "<p.moveDown>";        # j alt
          "e" = "<p.moveUp>";          # k alt
          "o" = "<p.moveRight>";       # l alt

          # Fast movement
          "Y" = "<p.moveFastLeft>";    # H alt
          "N" = "<p.moveFastDown>";    # J alt
          "E" = "<p.moveFastUp>";      # K alt
          "O" = "<p.moveFastRight>";   # L alt

          # Substitutions
          # Even more yank
          "cI" = "<p.copyImageBuffer>";            # yI sub
          "cT" = "<p.copyPageTitle>";              # yT sub
          "ca" = "<p.copyAudio>";                  # ya sub
          "cf" = "<p.copyFrame>";                  # yf sub
          "ci" = "<p.copyImage>";                  # yi sub
          "cl" = "<p.copyLink>";                   # yl sub
          "ct" = "<p.copyTitleAttr>";              # yt sub
          "cv" = "<p.copyVideo>";                  # yv sub
          "cy" = "<p.copyLink>";                   # yy sub
        } ++ [  # Unmapping
          # Normal Mode
          "nunmap yR<Any>"          # unmap <pageRSSLinkToClipboard>
          "nunmap yRL"              # unmap <pageRSSLinksList>
          "nunmap ye"               # unmap <pageToClipboardEmacs>
          "nunmap yf"               # unmap <startFollowCopyLink>
          "nunmap yh"               # unmap <pageToClipboardHTML>
          "nunmap ym"               # unmap <pageToClipboardMarkdown>
          "nunmap yr"               # unmap <pageToClipboardRST>
          "nunmap ys"               # unmap <p.copyText>
          "nunmap yt"               # unmap <pageTitleToClipboard>
          "nunmap yy"               # unmap <pageToClipboard>
          "nunmap F"                # unmap <startFollowNewTab>
          "nunmap c"                # unmap <p.start>

          # Pointer Mode
          "punmap oa"               # unmap <p.openAudio>
          "punmap of"               # unmap <p.openFrame>
          "punmap oi"               # unmap <p.openImage>
          "punmap ol"               # unmap <p.openLink>
          "punmap oo"               # unmap <p.openLink>
          "punmap ov"               # unmap <p.openVideo>
          "punmap yI"               # unmap <p.copyImageBuffer>
          "punmap yT"               # unmap <p.copyPageTitle>
          "punmap ya"               # unmap <p.copyAudio>
          "punmap yf"               # unmap <p.copyFrame>
          "punmap yi"               # unmap <p.copyImage>
          "punmap yl"               # unmap <p.copyLink>
          "punmap yt"               # unmap <p.copyTitleAttr>
          "punmap yv"               # unmap <p.copyVideo>
          "punmap yy"               # unmap <p.copyLink>
        ] ++ mapAttrsToList (n: v: "${n} ${v}")
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
      #neofetch="neofetch --ascii /home/bano/nixos/home-manager/NixOwOs.txt --ascii_colors 4 5";
      # GrainOS test
      neofetch="neofetch --ascii /home/bano/nixos/home-manager/GrainOS.txt --ascii_colors 3";

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
      estop="emacsclient -e '(kill-emacs)'";
      epkgs="nix-env -f '<nixpkgs>' -qaP -A emacsPackages";

      # LTE networking
      ltestart="sudo nmcli c add type gsm ifname '*' con-name NixPad-CON apn cdc-wdm0";
      lteon="nmcli r wwan on";
      lteoff="nmcli r wwan off";

      # Wireguard controls
      wgstart="sudo systemctl start wg-quick-wg0";
      wgstop="sudo systemctl stop wg-quick-wg0";

      # Discord RPC restart - until I figure out ZSH scripting
      rpc="systemctl --user restart mpd-discord-rpc";

      # Easier running of python scripts
      py="python";
    };

    # More advanced Z shell scripts - WIP
    zsh.enable = true;
    zsh.shellAliases = {
      # Vesktop with RPC restart
      vesktop = "${pkgs.writeShellScript "vesktop" ''
        vencorddesktop &
        systemctl --user restart mpd-discord-rpc &
      ''}";
    };

    # Rando numbers go brrr
    btop = {
      enable = true;
      settings = {
        color_theme = "kyli0x";
        theme_background = false;
        rounded_corners = false;
        graph_symbol = "block";
        clock_format = "/user on /host";
      };
    };
    
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
          epkgs.rustic

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

        alternative_header_first_line_format = "$b$4$aqqu$/a$9 $(220){%t}|{%f} $9$4$atqq$/a$9$/b";
        alternative_header_second_line_format = "{{$(14)$b%a$/b$9}{ - $(31)%b$9}{ ($(160)%y$9)}}|{%D}";
        
        main_window_color = 31;
        alternative_ui_separator_color = 31;
        window_border_color = 31;
        progressbar_color = 185;
        song_columns_list_format = "(20)[14]{a} (45)[220]{t|f:Title} (25)[31]{b} (7f)[160]{l}";

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
    #thefuck.enable = true;    - Disabled due to failed tests 23/03/24

    # Simple image viewing!
    feh.enable = true;

    # For recording my shenanigans
    obs-studio.enable = true;

    # Discord CSS injection!  - Disabled for now due to file access errors
    /*discocss = {
      enable = true;
      css = builtins.readFile(./BasicBackground.theme.css);
      discordAlias = false;
      discordPackage = pkgs.vesktop;
    };*/
    
    # Speedy fast app launcher
    fuzzel = {
      enable = true;
      settings = {
        main = {
          icons-enabled = "no";
          dpi-aware = "no";
          width = 30;
          font = "Hack:weight=bold:size=13";
          line-height = 22;
          fields = "name,generic,comment,categories,filename,keywords";
          terminal = "${pkgs.kitty}/bin/kitty";
          layer = "overlay";
        };
        colors = {
          background = "B55088FA";
          text = "FFFFFFFF";
          match = "FFAFFFFF";
          selection = "007F7FFF";
          selection-text = "99CCCCFF";
          selection-match = "004C4CFF";
          border = "FFAFFFFF";
        };
        border = {
          width = 4;
          radius = 0;
        };
      };
    };
    
    # Hyprland wallpaper
    wpaperd = {
      enable = true;
      settings = {
        eDP-1 = {
          path = "~/nixos/home-manager/hyprland/assets/wall2.png";
          apply-shadow = true;
        };
        HDMI-A-1 = {
          path = "~/nixos/home-manager/hyprland/assets/wall_anime_8K.png";
          apply-shadow = true;
        };
        DP-1 = {
          path = "~/nixos/home-manager/hyprland/assets/wall_anime2_8K.png";
          apply-shadow = true;
        };
        DP-2 = {
          path = "~/nixos/home-manager/hyprland/assets/wall_anime2_8K.png";
          apply-shadow = true;
        };
      };
    };

    # Hyprland status bar
    waybar = {
      enable = true;
      style = ./hyprland/style.css;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 34;
          output = [
            "eDP-1"
            "HDMI-A-1"
            "DP-1"
            "DP-2"
          ];

          modules-left = ["hyprland/workspaces" "user" "disk"];
          modules-center = ["hyprland/window"];
          modules-right = ["cpu" "memory" "temperature" "battery" "network" "clock" "tray"];

          "hyprland/workspaces" = {
            format = "{name}";
            icon = true;
          };

          "user" = {
            format = "as bano 🍌 | up {work_H} hrs >:3c";
          };

          "disk" = {
            interval = 30;
            format = "| {percentage_used}% capacity UwU";
            path = "/";
          };

          "hyprland/window" = {
            format = "messin wit {class} | on {title} | NixOwOs btw  ";
          };

          "cpu" = {
            states = {
              good = 0;
              warning = 40;
              critical = 80;
            };
            format = "{usage}% ";
            tooltip = false;
          };

          "memory" = {
            states = {
              good = 0;
              warning = 40;
              critical = 80;
            };
            format = "{} ";
          };

          "temperature" = {
            # thermal-zone = 2;
            # hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
            critical-threshold = 70;
            format-critical = "{temperatureC}°C ";
            format = "{temperatureC}°C ";
          };

          "battery" = {
            states = {
              full = 100;
              good = 70;
              warning = 30;
              critical = 10;
            };
            format-charging = "{capacity}% ";
            format-plugged = "{capacity}% ";
            format-full = "{time} {capacity}% ";
            format-good = "{time} {capacity}% ";
            format-warning = "{time} {capacity}% ";
            format-critical = "{time} {capacity}% ";
          };

          "network" = {
            format-wifi = "{signalStrength}% ";
            format-ethernet = "eth ";
            format-disconnected = "dc ";
          };
          
          "clock" = {
            format = "{:%I:%M   | %d/%m/%Y }";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          };
        };
      };
    };
  };
  
  # Ensuring gpg has access to pinentry
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-qt;
  };

  # For running Emacs as daemon
  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
  };

  services.cliphist.enable = true;

  services.mpd-discord-rpc = {
    enable = true;
    settings = {
      format = {
        details = "Weebing to $title";
        state = "By $artist, from $album";
      };
    };
  };

  services.dunst = {
    enable = true;
    settings = {
      global = {
        follow = "mouse";
        width = 300;
        height = 300;
        offset = "44x49";
        origin = "top-right";
        progress_bar = true;
        progress_bar_height = 20;
        frame_color = "#FFAFFF";
        font = "JetBrains Mono 11";
      };

      urgency_normal = {
        background = "#B55088";
        foreground = "#FFFFFF";
        timeout = 10;
      };
    };
  };
  
  wayland.windowManager.hyprland = {
     enable = true;

    extraConfig = ''
      monitor = DP-2, highres, 0x0, 1
      monitor = DP-1, highres, -240x0, 1
      monitor = HDMI-A-1, highres, 1680x0, 1
      monitor = eDP-1, highres, 3600x0, 1

      exec-once = wpaperd
      exec-once = waybar
      exec-once = kitty
      exec-once = dunst

      exec-once = wl-paste --type text --watch cliphist store #Stores only text data
      exec-once = wl-paste --type image --watch cliphist store #Stores only image data

      env = LIBVA_DRIVER_NAME, nvidia
      env = XDG_SESSION_TYPE, wayland
      env = GBM_BACKEND, nvidia-drm
      env = __GLX_VENDOR_LIBRARY_NAME, nvidia
      env = WLR_NO_HARDWARE_CURSORS, 1
    '';

    settings = {
      "$mod" = "SUPER";
      general = {
        "border_size" = 3;
        "col.active_border" = "rgb(FFAFFF)";
        "col.inactive_border" = "rgb(007F7F)";
      };
      decoration = {
        "active_opacity" = 0.9;
        "inactive_opacity" = 0.9;
        blur = {
          "noise" = 0.1;
        };
      };
      # Volume control with optional press and hold
      binde =
        [
          ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 1%+"
          ", XF86AudioLowerVolume, exec, wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 1%-"
        ];
      bind =
        [
          # Global actions
          "$mod, Q, killactive"
          "$mod, Tab, cyclenext"
          "$mod, F, fullscreen, 0"

          # Copying
          "$mod, S, exec, grimblast --cursor copy output"
          "$mod SHIFT, S, exec, grimblast copy area"
          "$mod, P, exec, hyprpicker -n -a"
          "$mod, V, exec, cliphist list | fuzzel --dmenu | cliphist decode | wl-copy"

          # Workspace motions
          "$mod, Y, movewindow, l"
          "$mod, N, workspace, -1"
          "$mod, E, workspace, +1"
          "$mod, O, movewindow, r"

          # Program specific actions
          "$mod, Return, exec, kitty"
          "$mod, Space, exec, fuzzel"
          "$mod, D, exec, vesktop"
          "$mod, H, exec, emacsclient --create-frame ~/nixos/home-manager/home.nix"
          "$mod, B, exec, vieb"
        ]
        ++ (
          # Workspaces
          # Binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
          builtins.concatLists (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in [
              "$mod, ${ws}, workspace, ${toString (x + 1)}"
              "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
            ]
          )
          10)
        );
    };
  };
}
