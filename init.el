(add-to-list 'load-path "~/.emacs.d")

;; fix lame defaults
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
;; delete/backspace are not the same
(normal-erase-is-backspace-mode 1)
;; end of file infinite scrolling
(setq next-line-add-newlines nil)
;; disable half-screen jumps
(setq scroll-step 1)
(setq make-backup-files nil)
;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; disable bell
(setq visible-bell t)
;; recent files
(require 'recentf)
    (recentf-mode 1)
(set-face-attribute 'default nil :height 150)

;; highlight over 80
;;(require 'whitespace)
;;(setq my-long-line-face (make-face 'my-long-line-face))
;;(set-face-attribute 'my-long-line-face nil :underline t)
;;(setq whitespace-line 'my-long-line-face)
;;(setq whitespace-style '(face lines-tail))
;;(global-whitespace-mode t)

;; (setq whitespace-style '(trailing lines space-before-tab
;;                                   indentation space-after-tab) )


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
;;(add-hook 'ruby-mode-hook (lambda () (interactive) (column-marker-3 80)) )

;; yaml syntax highlighting
(require 'yaml-mode)
(require 'tt-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))
(add-to-list 'auto-mode-alist '("\\.ett$" . tt-mode))

;; maximum frame on launch
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)


;; better backup system
(require 'ebackup)

(require 'ruby-block)
(ruby-block-mode t)

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

;; autocomplete
(add-to-list 'load-path "~/.emacs.d/")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;; (ac-config-default)



;; cucumber
;; Copy files to ~/.emacs.d/elisp/feature-mode, for example,
;; and add this to your .emacs to load the mode
(add-to-list 'load-path "~/.emacs.d/feature-mode")
;; ;; optional configurations
;; ;; default language if .feature doesn't have "# language: fi"
;(setq feature-default-language "fi")

;; point to cucumber languages.yml or gherkin i18n.yml to use
;; exactly the same localization your cucumber uses
(setq feature-default-i18n-file "/Users/pvaughn/.rvm/gems/ruby-1.9.3-p125/gems/gherkin-2.11.1/lib/gherkin/i18n.yml")
;; ;; and load feature-mode
(require 'feature-mode)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("content-type" . nxml-mode))

(autoload 'dirtree "dirtree" "Add directory to tree view" t)