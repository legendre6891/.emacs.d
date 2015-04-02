;; ============================================================================
;; The first order of business is to
;; install use-package

(require 'cl)


(prefer-coding-system 'utf-8) ;; so we don't get bothered about coding system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; ============================================================================


;; ============================================================================
;; Next, install the necessary packages



;; ======================================================================
;; Minimal defaults to make emacs a comfortable working environment

(use-package better-defaults
  :ensure t
  :config
  (progn
    (setq ring-bell-function 'ignore)
    (setq inhibit-startup-message t)
    (setq ido-use-virtual-buffers t)
    (setq ido-everyhwere t)
    (setq ido-auto-merge-work-directories-length -1)
    ;; Do garbage collection less and less
    (setq gc-cons-threshold 50000000)))

(use-package flx-ido
  :ensure t
  :idle (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :idle (ido-vertical-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :init
  (progn
    (ido-ubiquitous-mode 1)
    ;; Fix ido-ubiquitous for newer packages
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
  :init
  (recentf-mode 1)
  :config
  (progn
    (setq recentf-max-menu-items 500)))

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("C-x C-m" . smex)
   ("C-c C-m" . smex)
   ("C-c m" . smex)
   ("C-x m" . smex)))

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 110
                    :weight 'normal
                    :width 'normal)


(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-eighties t))

;; ======================================================================
;; Evil mode setup

(use-package evil-leader
  :ensure t
  :init
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode 1))
  :config
  (progn
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
  :init (evil-mode 1)
  :config
  (progn
    (setq evil-default-cursor t)

    (define-key evil-insert-state-map "\C-e" 'end-of-line)
    (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
    (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
    (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
    (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
    (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
    (define-key evil-insert-state-map "\C-n" 'evil-next-line)
    (define-key evil-visual-state-map "\C-n" 'evil-next-line)
    (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
    (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
    (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word)
    (define-key evil-visual-state-map "\C-w" 'evil-delete)
    (define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
    (define-key evil-visual-state-map "\C-u" 'evil-scroll-up)
    (define-key evil-normal-state-map "\C-y" 'yank)
    (define-key evil-insert-state-map "\C-y" 'yank)
    (define-key evil-visual-state-map "\C-y" 'yank)
    (define-key evil-insert-state-map "\C-k" 'kill-line)
    (define-key evil-visual-state-map "\C-k" 'kill-line)
    (define-key evil-normal-state-map "0" 'evil-beginning-of-line)
    (define-key evil-normal-state-map "H" 'evil-first-non-blank)
    (define-key evil-normal-state-map "L" 'evil-end-of-line)
    (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-normal-state-map "0" 'evil-beginning-of-line)
    (define-key evil-visual-state-map "H" 'evil-first-non-blank)
    (define-key evil-visual-state-map "L" 'evil-end-of-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    (define-key evil-normal-state-map "j" 'evil-next-visual-line)
    (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
    (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
    (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "fd" 'evil-normal-state))
  :diminish undo-tree-mode)


(use-package evil-surround
  :ensure t
  :idle (global-evil-surround-mode 1)
  :config
  (progn
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

(use-package evil-commentary
  :ensure t
  :idle
  (evil-commentary-mode 1)
  :bind
  ("M-;" . evil-commentary-line)
  :diminish evil-commentary-mode)

;; ======================================================================
;; yasnippet

(use-package yasnippet
  :ensure t
  :pre-load (setq yas-snippet-dirs `(,(expand-file-name "~/.emacs.d/snippets")))
  :init (yas-reload-all)
  :idle-priority 1
  :config
  (progn
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
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
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
    (setq TeX-PDF-mode t)

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
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
    (setq cdlatex-paired-parens "$[{(")
    (setq cdlatex-command-alist
          '(
            ("bi" "Insert \\binom{}{}" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
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
            ("pr" "Insert proof env" "" cdlatex-environment ("proof") t nil)
            ))
    (setq cdlatex-math-modify-alist
          '(
            (?t "\\text" nil t nil nil)
            (?s "\\mathscr" nil t nil nil)
            ))
    (setq cdlatex-env-alist
          '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
            ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
            ("lemma" "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
            ("claim" "\\begin{claim}\nAUTOLABEL\n?\n\\end{claim}\n" nil)
            ("proposition" "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
            ("wts" "\\begin{wts}\nAUTOLABEL\n?\n\\end{wts}\n" nil)
            ("definition" "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil)
            )))
  :diminish cdlatex-mode)

(use-package smartparens-config
  :ensure smartparens
  :config (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  :diminish smartparens-mode)

(use-package latex-extra
  :ensure t
  :commands (latex-extra-mode latex/compile-commands-until-done)
  :init (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  :config
  (progn
    (add-hook 'LaTeX-mode-hook
              (local-set-key (kbd "<f5>")
                         '(lambda ()
                            (interactive)
                            (save-buffer)
                            (call-interactively 'latex/compile-commands-until-done))))))

(use-package legendre-latex
  :load-path "lisp/")



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

;; ======================================================================
;; paredit hacking

(use-package paredit
  :ensure t
  :diminish paredit-mode
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

;; ======================================================================

(use-package magit
  :ensure t
  :defer t)

;; ======================================================================
;; Other goodies

(use-package fill-column-indicator
  :ensure t
  :commands turn-on-fci-mode
  :init
  (progn
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (global-fci-mode 1)))


;; ======================================================================

