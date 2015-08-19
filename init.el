;; ===========================================================================
;; The first order of business is to
;; install use-package
(require 'cl)
(require 'org)

(prefer-coding-system 'utf-8) ;; so we don't get bothered about coding system

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ========================
;; Load the custom file (create it first if it doesn't exist)
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))

(load custom-file)

;; Always make sure that we have
;; full-name and email-address
;; as part of our configuration.
(defun ensure-in-custom (SYM PROMPT)
  (interactive)
  (if (not (boundp SYM))
      (let* ((entry (read-from-minibuffer PROMPT))
             (symbol (symbol-name SYM))
             (piece `(setq ,SYM ,entry)))
        (eval piece)
        (write-region (concat (prin1-to-string piece) "\n")
         nil custom-file 'append)
        )))

(ensure-in-custom 'full-name "Enter your full name: ")
(ensure-in-custom 'email-address "Enter your email address: ")
;; ===========================================================================
;; Home folder names and common paths
;; I use the word "folder" here as to not conflict with
;; built-in emacs variables, which uses "dir".

(setq user-home-folder
      (file-name-as-directory
       (if (eq system-type 'windows-nt)
           (substitute-in-file-name "$HOMEDRIVE$HOMEPATH")
           "~")))

(setq user-dropbox-folder
      (file-name-as-directory
       (concat user-home-folder "Dropbox")))
;; ===========================================================================

;; ===========================================================================
;; Some nasty things to get things working on the mac
(if (eq system-type 'darwin)
    (progn
      (defun set-exec-path-from-shell-PATH ()
        (let ((path-from-shell
               (shell-command-to-string
                "TERM=vt100 $SHELL -i -c 'echo $PATH'")))

          (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))

      (when window-system (set-exec-path-from-shell-PATH))

      (getenv "PATH")
      (setenv "PATH" (concat "/usr/texbin" ":" (getenv "PATH")))

      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
      (setq exec-path (append exec-path '("/usr/local/bin")))

      (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
      (setq exec-path (append exec-path '("/usr/bin")))))
;; ========================================================================

;; ========================================================================
;; Next, install the necessary packages

;; Minimal defaults to make emacs a comfortable working environment
(use-package better-defaults
    :ensure t
    :init
    (progn
      (setq ring-bell-function 'ignore)
      (setq inhibit-startup-message t)
      (setq ido-use-virtual-buffers t)
      (setq ido-everyhwere t)
      (setq ido-auto-merge-work-directories-length -1)
      (setq lisp-indent-function 'common-lisp-indent-function)
      (setq-default tab-width 4) ; or any other preferred
      (defvaralias 'c-basic-offset 'tab-width)
      (defvaralias 'cperl-indent-level 'tab-width)
      (defalias 'yes-or-no-p 'y-or-n-p)
      (column-number-mode t)
      (setq set-mark-command-repeat-pop t)
      (setq gc-cons-threshold 50000000)))

(use-package flx-ido
    :ensure t
    :config (flx-ido-mode 1))

(use-package ido-vertical-mode
    :ensure t
    :config
    (progn
      (ido-vertical-mode 1)
      (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
        (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
      (add-hook 'ido-setup-hook 'ido-define-keys)))

(use-package ido-ubiquitous
    :ensure t
    :init
    (progn
      (ido-ubiquitous-mode 1)
      (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
        `(eval-after-load ,package
           '(defadvice ,cmd (around ido-ubiquitous-new activate)
             (let ((ido-ubiquitous-enable-compatibility nil))
               ad-do-it)))))
    :config
    (progn
      (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
      (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)))

(use-package recentf
    :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 500)))

(use-package smex
    :ensure t
    :bind
    (("M-x" . smex)
     ("C-x C-m" . smex)
     ("C-c C-m" . smex)
     ("C-c m" . smex)
     ("C-x m" . smex)))

(use-package bug-hunter
    :ensure t)

;; ===================================================================
;; Color Themes and font
(use-package color-theme-sanityinc-tomorrow
    :defer t
    :ensure t)
(use-package ample-theme
    :defer t
    :ensure t)
(use-package smyx-theme
    :defer t
    :ensure t)
(use-package aurora-theme
    :defer t
    :ensure t)

(use-package smart-mode-line
    :ensure smart-mode-line
    :init
    (progn
      (setq sml/theme 'light)
      (setq sml/name-width 40)
      (setq sml/mode-width 'full)
      )
    :config
    (progn
      (sml/setup)))

(use-package nlinum
    :ensure t
    :init
    (global-nlinum-mode t))

(load-theme 'aurora t)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; ======================================================================
;; evil mode
;; (load "~/.emacs.d/lisp/evil.el")
;; ======================================================================


;; ======================================================================
;; yasnippet

(use-package yasnippet
    :ensure t
    :config
    (progn
      (yas-reload-all)
      (define-key yas-minor-mode-map (kbd "<tab>") nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      (define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)
      (define-key yas-keymap [(tab)]       nil)
      (define-key yas-keymap (kbd "TAB")   nil)
      (define-key yas-keymap (kbd "C-j") 'yas-prev-field)
      (define-key yas-keymap (kbd "C-;") 'yas-next-field-or-maybe-expand)
      (add-hook 'emacs-lisp-mode-hook '(lambda () (yas-minor-mode)))
      (add-hook 'LaTeX-mode-hook '(lambda () (yas-minor-mode)))
      (add-hook 'TeX-mode-hook '(lambda () (yas-minor-mode))))
    :diminish yas-minor-mode)

;; ======================================================================
;; Latex
;; ======================================================================
(load-file "~/.emacs.d/lisp/my-latex.el")

(use-package smartparens-config
    :ensure smartparens
    :config (progn
              (add-hook 'TeX-mode-hook 'smartparens-mode)
              (add-hook 'LaTeX-mode-hook 'smartparens-mode))
    :diminish smartparens-mode)

;; (use-package legendre-latex-keys
;;     :load-path "~/.emacs.d/lisp/")
;; ======================================================================
;; Other utilties for a better life

(use-package nlinum
    :ensure t
    :commands nlinum-mode)
(use-package s
    :ensure t)
(use-package dash
    :ensure t)
(use-package company
    :ensure t
    :config
    (progn
      (let ((map company-active-map))
        (define-key map (kbd "C-/") 'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer)
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-n") 'company-select-next)
        (define-key map (kbd "C-p") 'company-select-previous)
        (define-key map (kbd "C-l") 'company-complete-selection))
       ))
;; ======================================================================
;; lisp hacking
(use-package paredit
    :ensure t
    :config
    (progn
      (add-hook 'clojure-mode-hook 'enable-paredit-mode)
      (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
      (add-hook 'lisp-mode-hook 'enable-paredit-mode)
      (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
      (add-hook 'ielm-mode-hook 'enable-paredit-mode)
      (add-hook 'json-mode-hook 'enable-paredit-mode)))

(use-package geiser
    :ensure t
    :defer t
    :config
    (progn
      (setq scheme-progrma-name "racket")
      (setq geiser-impl-installed-implementations '(racket))))

(use-package clojure-mode
    :ensure t)

(use-package cider
    :ensure t
    :init
    (add-hook 'cider-mode-hook 'eldoc-mode))

;; ======================================================================
(use-package magit
    :ensure t
    :config (setq magit-last-seen-setup-instructions "1.4.0")
    :defer t)

;; ======================================================================
;; Other goodies
(use-package fill-column-indicator
    :ensure t
    :config
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "darkblue")
      (add-hook 'LaTeX-mode-hook '(lambda () (fci-mode 1)))))
(use-package paradox
    :ensure t)
(use-package async
    :ensure t)
(use-package reveal-in-osx-finder
    :ensure t)
(use-package fuzzy-format
    :ensure t
    :config
    (progn
      (setq fuzzy-format-default-indent-tabs-mode nil)
      (global-fuzzy-format-mode t))
    :diminish fuzzy-format-mode)

(use-package guide-key
    :ensure t
    :init
    (progn
      (setq guide-key/guide-key-sequence '("C-c")))
    :config
    (guide-key-mode t))
;; ======================================================================
;; Other programming languages
(use-package julia-mode
    :ensure t)

(use-package j-mode
    :ensure t
    :config (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

(load "~/.emacs.d/lisp/cplusplus.el")
;; ======================================================================
;; Org mode changes

(use-package htmlize
    :ensure t)

(use-package org
    :ensure t
    :init
    (progn
      (setq org-src-fontify-natively t)
      (setq org-catch-invisible-edits 'smart)
      (setq org-list-allow-alphabetical t)

      ;; Set the org directory
      (let ((org-folder
             (file-name-as-directory (concat user-dropbox-folder "ORG"))))
        (setq org-directory org-folder))

      ;; Set the global hotkeys are recommended.
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cc" 'org-capture)
      (global-set-key "\C-cb" 'org-iswitchb))

    ;; Easier LaTeX switching
    :bind (("C-." . org-toggle-latex-fragment))
    :config
    (progn
      (plist-put org-format-latex-options :scale 1.4)
      (plist-put org-format-latex-options :html-scale 1.4)))
;; =================================================

