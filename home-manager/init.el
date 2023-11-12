;; Theme setup
(add-to-list 'load-path "~/home-manager/manual-packages/ef-themes")
(require 'ef-themes)
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)
;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)
;; Load the theme of choice:
(load-theme 'ef-cherie :no-confirm)


;; Major settings
(setq inhibit-startup-message t)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)


;; UI setup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)


;; General bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Package init
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)


;; Minimum package setup
(use-package command-log-mode)
(use-package nerd-icons-completion)
(use-package nerd-icons-dired)
(use-package nerd-icons-ibuffer)
(use-package nerd-icons-ivy-rich)


;; Enable line numbers globally
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :demand
  :config
  (evil-mode 1)
  ;; Extra vi-like bindings
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Set states
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; Ivy
(use-package ivy
  :diminish
  :demand
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; Ivy Rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))


;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; General
(use-package general
  :config
  (general-create-definer bano/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (bano/leader-keys
    "t"  '(:ignore t :which-key "toggles")))


;; Nerd Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))


;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))
(setq nerd-icons-scale-factor 1.5)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq find-file-visit-truename t)


;; Hydra
(use-package hydra)
(defhydra hydra-text-scale ()
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


;; Leader keys for General
(bano/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(counsel-load-theme :which-key "choose theme"))
