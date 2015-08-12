;; ===================================================================
;; Evil mode setup
;; ===================================================================

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
