(add-to-list 'load-path "~/.emacs.d/lisp")

;; fix lame defaults
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
;; delete/backspace are not the same
(normal-erase-is-backspace-mode 1)
;; end of file infinite scrolling
(setq next-line-add-newlines nil)
;; disable half-screen jumps
(setq scroll-step 1)
;; disable backup
(setq make-backup-files nil)
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; disable bell
(setq visible-bell t)
;; disable file locks (.#files)
(setq create-lockfiles nil)

;; bump up font size
(set-face-attribute 'default nil :height 150)

;; delete trailing whitespace
(require 'ws-trim)
(global-ws-trim-mode t)

;; show cursor position
(column-number-mode)

;; better kill-ring
(require 'browse-kill-ring)

;; key bindings
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key [(control l)] 'goto-line)
(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)] 'end-of-line)


;; tab behaviors
;; no tabs, only spaces
(setq-default indent-tabs-mode nil)
;; default width
(setq-default tab-width 4)
;; tabs are 2 spaces wide for ruby code
(add-hook 'ruby-mode-hook (lambda () (setq tab-width 2)))

;; yaml syntax highlighting
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; XML
(add-to-list 'auto-mode-alist '("\.xslt" . nxml-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(require 'ruby-block)
(ruby-block-mode t)

;; maximum frame on launch
(require 'maxframe)
(setq mf-max-width 1440)
(add-hook 'window-setup-hook 'maximize-frame t)

;; better backup system
(setq ebackup-destination-dir "~/.emacs.d/backups")
(require 'ebackup)
(setq ebackup-destination-dir "~/.emacs.d/backups")


;; color themes
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;; gray 
     ;; (color-theme-calm-forest)
     ;; green
     ;; (color-theme-subtle-hacker)  
     ;; black
     ;; (color-theme-arjen)
     ;; (color-theme-comidia)
     (color-theme-hober)
     ;; (color-theme-renegade)
     ;; (color-theme-taming-mr-arneson)
     ;; (color-theme-tty-dark)
     ;; white
     ;; (color-theme-black)

     ))

(autoload 'artist-mode "artist" "Enter artist-mode" t)


;; cucumber
;; Copy files to ~/.emacs.d/elisp/feature-mode, for example,
;; and add this to your .emacs to load the mode
;;(add-to-list 'load-path "~/.emacs.d/feature-mode")
;; ;; optional configurations
;; ;; default language if .feature doesn't have "# language: fi"
;(setq feature-default-language "fi")
;; point to cucumber languages.yml or gherkin i18n.yml to use
;; exactly the same localization your cucumber uses
;;(setq feature-default-i18n-file "/Users/pvaughn/.rvm/gems/ruby-1.9.3-p125/gems/gherkin-2.11.1/lib/gherkin/i18n.yml")
;; and load feature-mode
;;(require 'feature-mode#)
;;(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

