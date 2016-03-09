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

(set-default 'truncate-lines t)

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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Unbind Pesky Sleep Button
(when (display-graphic-p)
  (global-unset-key [(control z)])
  (global-unset-key [(control x)(control z)]))

;; tab behaviors
;; no tabs, only spaces
(setq-default indent-tabs-mode nil)
;; default width
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
;;(add-hook 'ruby-mode-hook (lambda () (setq tab-width 2)))

;; less syntax support
(require 'less-css-mode)

;; yaml syntax highlighting
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; XML
(add-to-list 'auto-mode-alist '("\\.xslt" . nxml-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(require 'ruby-block)
(ruby-block-mode t)
;; don't insert file encoding
(setq ruby-insert-encoding-magic-comment nil)

;; Go
(require 'go-mode-autoloads)
(add-to-list 'auto-mode-alist '("\\.go" . go-mode))

;; maximum frame on launch
(require 'maxframe)
(setq mf-max-width 1440)
(add-hook 'window-setup-hook 'maximize-frame t)

;; better backup system
(setq ebackup-destination-dir "~/.emacs.d/backups")
(require 'ebackup)
(setq ebackup-destination-dir "~/.emacs.d/backups")

(require 'erlang)

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

(require 'doc-mode)
(require 'asciidoc)


(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-hook 'doc-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (require 'asciidoc)))
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

(require 'ibuffer-vc)
(add-hook 'ibuffer-hook
  (lambda ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic))))


 (setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))


;; handy function to colorize color-coded files
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Ignore modification-time-only changes in files, i.e. ones that
;; don't really change the contents.  This happens often with
;; switching between different VC buffers.

(defun update-buffer-modtime-if-byte-identical ()
  (let* ((size      (buffer-size))
         (byte-size (position-bytes size))
         (filename  buffer-file-name))
    (when (and byte-size (<= size 1000000))
      (let* ((attributes (file-attributes filename))
             (file-size  (nth 7 attributes)))
        (when (and file-size
                   (= file-size byte-size)
                   (string= (buffer-substring-no-properties 1 (1+ size))
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (buffer-string))))
          (set-visited-file-modtime (nth 5 attributes))
          t)))))

(defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
  (or (funcall original buffer)
      (with-current-buffer buffer
        (update-buffer-modtime-if-byte-identical))))
(advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

(defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
  (unless (update-buffer-modtime-if-byte-identical)
    (apply original arguments)))
(advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supersession-threat--ignore-byte-identical)
(put 'upcase-region 'disabled nil)
