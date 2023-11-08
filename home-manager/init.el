;; Make Elisp files in that directory available to the user.
;;(add-to-list 'load-path "~/home-manager/manual-packages/ef-themes")


;; Theme setup
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
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)


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


;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
