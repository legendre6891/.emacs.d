;; ============================================================================
;; The first order of business is to
;; install use-package
(require 'cl)

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


;; ============================================================================
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(getenv "PATH")
(setenv "PATH" (concat "/usr/texbin" ":" (getenv "PATH")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setq exec-path (append exec-path '("/usr/bin")))

(defalias 'yes-or-no-p 'y-or-n-p)
;; ============================================================================

;; ============================================================================
;; Next, install the necessary packages

;; Minimal defaults to make emacs a comfortable working environment
(use-package better-defaults
    :ensure t
    :init
    (progn
      (global-nlinum-mode t)
      (setq ring-bell-function 'ignore)
      (setq inhibit-startup-message t)
      (setq ido-use-virtual-buffers t)
      (setq ido-everyhwere t)
      (setq ido-auto-merge-work-directories-length -1)
      (setq lisp-indent-function 'common-lisp-indent-function)
      (setq-default tab-width 4) ; or any other preferred
      (defvaralias 'c-basic-offset 'tab-width)
      (defvaralias 'cperl-indent-level 'tab-width)
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

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 140
                    :weight 'normal
                    :width 'normal)


;; ==============
;; evil mode
;; (load "~/.emacs.d/lisp/evil.el")
;; ===========

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
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cc" 'org-capture)
      (global-set-key "\C-cb" 'org-iswitchb)))
;; =================================================


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list
   (quote
    (("Skim"
      (concat "/Applications/Skim.app/" "Contents/SharedSupport/displayline" " %n %o %b")))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   (quote
    ("f8c697230b77a70f903401d97fe3d86c3f60461f39a2c925824dadba7354c495" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "eafda598b275a9d68cc1fbe1689925f503cab719ee16be23b10a9f2cc5872069" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" default)))
 '(paradox-github-token t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



