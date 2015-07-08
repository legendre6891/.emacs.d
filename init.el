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
      (setq ring-bell-function 'ignore)
      (setq inhibit-startup-message t)
      (setq ido-use-virtual-buffers t)
      (setq ido-everyhwere t)
      (setq ido-auto-merge-work-directories-length -1)
      (setq lisp-indent-function 'common-lisp-indent-function)
      (setq gc-cons-threshold 50000000)))

(use-package flx-ido
    :ensure t
    :config (flx-ido-mode 1))

(use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode 1))

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

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 140
                    :weight 'normal
                    :width 'normal)
(load-theme 'smyx 1)

;; ===================================================================
;; Evil mode setup

(use-package evil-leader
    :ensure t
    :config
    (progn
      (evil-leader/set-leader "<SPC>")
      (global-evil-leader-mode 1)
      (evil-leader/set-key
          "SPC" 'smex
        "u" 'undo-tree-visualize
        ";" 'evil-commentary-line
        "t m" 'menu-bar-mode
        "t h" 'hl-line-mode
        "t n" 'nlinum-mode
        "e v" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
        "t l" 'nlinum-mode)))

(use-package key-chord
    :ensure t
    :init (key-chord-mode 1))

(use-package evil
    :ensure t
    :load-path "~/.emacs.d/lisp/"
    :init (evil-mode 1)
    :config
      (load "~/.emacs.d/lisp/evil-shortcuts.el")
    :diminish undo-tree-mode)

(use-package evil-surround
    :ensure t
    :config
    (progn
      (global-evil-surround-mode 1)
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

(use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode 1)
    :bind
    ("M-;" . evil-commentary-line)
    :diminish evil-commentary-mode)

(use-package vim-hydra
    :load-path "~/.emacs.d/lisp")

(use-package latex-hydra
    :load-path "~/.emacs.d/lisp")
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
      (add-hook 'LaTeX-mode-hook '(lambda () (yas-minor-mode))))
    :diminish yas-minor-mode)

;; ======================================================================
;; latex!!

(use-package s
    :ensure t)

(use-package reftex ; TeX/BibTeX cross-reference management
    :defer t
    :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
    :config
    (progn
      ;; Plug into AUCTeX
      (setq reftex-plug-into-AUCTeX t
            reftex-label-alist
            '(("axiom" ?a "ax:" "~\\ref{%s}" t ("axiom" "ax.") -2)
              ("theorem" ?h "thr:" "~\\ref{%s}" t ("theorem" "th.") -3)
              ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "le.") -3)
              ("claim" ?m "clm:" "~\\ref{%s}" t ("claim" "cl.") -3)
              ("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition" "pr.") -3)
              ("wts" ?w "wts:" "~\\ref{%s}" t ("wts" "wt.") -3)
              ("definition" ?d "defn:" "~\\ref{%s}" t ("definition" "de.") -3)))
      ;; Provide basic RefTeX support for biblatex
      (unless (assq 'biblatex reftex-cite-format-builtin)
        (add-to-list 'reftex-cite-format-builtin
                     '(biblatex "The biblatex package"
                       ((?\C-m . "\\cite[]{%l}")
                        (?t . "\\textcite{%l}")
                        (?a . "\\autocite[]{%l}")
                        (?p . "\\parencite{%l}")
                        (?f . "\\footcite[][]{%l}")
                        (?F . "\\fullcite[]{%l}")
                        (?x . "[]{%l}")
                        (?X . "{%l}"))))
        (setq reftex-cite-format 'biblatex)))
    :diminish reftex-mode)

(use-package auctex
    :ensure t
    :commands (latex-mode LaTeX-mode plain-tex-mode LaTeX-mode-map)
    :config
    (progn
      (setq TeX-parse-self t)			; Enable parse on load.
      (setq TeX-auto-save t)			; Enable parse on save.
      (setq TeX-PDF-mode t)
      (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
      (add-hook 'LaTeX-mode-hook '(lambda () (fci-mode 1)))
      (add-hook 'LaTeX-mode-hook '(lambda () (electric-indent-mode -1)))
      (add-hook 'LaTeX-mode-hook
                (lambda ()
                  (LaTeX-add-environments
                   '("axiom" LaTeX-env-label)
                   '("theorem" LaTeX-env-label)
                   '("lemma" LaTeX-env-label)
                   '("claim" LaTeX-env-label)
                   '("proposition" LaTeX-env-label)
                   '("wts" LaTeX-env-label)
                   '("definition" LaTeX-env-label)
                   )))
      (setq TeX-output-view-style '("^pdf$" "." "SumatraPDF.exe -reuse-instance %o"))
      (setq TeX-view-program-list
            '(("SumatraPDF" "SumatraPDF.exe -reuse-instance %o")
              ))
      (cond
        ((eq system-type 'windows-nt)
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
                                                        (output-dvi "Yap"))))))
        ((eq system-type 'gnu/linux)
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (setq TeX-view-program-selection '((output-pdf "evince")
                                                        (output-dvi "evince")))))))))
(use-package cdlatex
    :ensure t
    :config
    (progn
      (add-hook 'TeX-mode-hook 'turn-on-cdlatex)
      (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
      (setq cdlatex-paired-parens "$[{(")
      (setq cdlatex-command-alist
            '(("bi" "Insert \\binom{}{}" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
              ("ggr(" "Insert \biggl( \biggr)" "\\biggl(? \\biggr" cdlatex-position-cursor nil nil t)
              ("ggr|" "Insert \biggl| \biggr|" "\\biggl|? \\biggr|" cdlatex-position-cursor nil nil t)
              ("ggr{" "Insert \biggl\{ \biggr\}" "\\biggl\\{? \\biggr\\" cdlatex-position-cursor nil nil t)
              ("ggr[" "Insert \biggl[ \biggr]" "\\biggl[? \\biggr" cdlatex-position-cursor nil nil t)
              ("ce" "Insert ceilings" "\\lceil? \\rceil" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\lfloor? \\rfloor" cdlatex-position-cursor nil nil t)
              ("ggrce" "Insert ceilings" "\\biggl\\lceil? \\biggr\\rceil" cdlatex-position-cursor nil nil t)
              ("int" "Insert integrals without limits" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
              ("sum" "Insert sums without limits" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prod" "Insert products without limits" "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prodl" "Insert products" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\biggl\\lfloor? \\biggr\\rfloor" cdlatex-position-cursor nil nil t)
              ("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
              ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
              ("lem" "Insert lemma env" "" cdlatex-environment ("lemma") t nil)
              ("clm" "Insert claim env" "" cdlatex-environment ("claim") t nil)
              ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
              ("wts" "Insert want to show env" "" cdlatex-environment ("wts") t nil)
              ("def" "Insert definition env" "" cdlatex-environment ("definition") t nil)
              ("pr" "Insert proof env" "" cdlatex-environment ("proof") t nil)))
      (setq cdlatex-math-modify-alist
            '((?t "\\text" nil t nil nil)
              (?s "\\mathscr" nil t nil nil)))
      (setq cdlatex-env-alist
            '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
              ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
              ("lemma" "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
              ("claim" "\\begin{claim}\nAUTOLABEL\n?\n\\end{claim}\n" nil)
              ("proposition" "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
              ("wts" "\\begin{wts}\nAUTOLABEL\n?\n\\end{wts}\n" nil)
              ("definition" "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil))))
    :diminish cdlatex-mode)

(use-package smartparens-config
    :ensure smartparens
    :config (progn
              (add-hook 'TeX-mode-hook 'smartparens-mode)
              (add-hook 'LaTeX-mode-hook 'smartparens-mode))
    :diminish smartparens-mode)

(use-package legendre-latex-keys
    :load-path "~/.emacs.d/lisp/")

;; ======================================================================
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
      (global-company-mode)))

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
;; ======================================================================
;; Other programming languages
(use-package julia-mode
    :ensure t)

(load "~/.emacs.d/lisp/jemdoc.el")
;; ======================================================================



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" default)))
 '(paradox-github-token t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


