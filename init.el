
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name
  "my-init.org"
  (concat user-emacs-directory "org")))

(put 'downcase-region 'disabled nil)



(add-to-list 'load-path "~/.emacs.d/org-ref/")
(use-package dash
    :ensure t)
(use-package helm
    :ensure t)

(use-package helm-config)

(use-package helm-bibtex
    :ensure t)

(use-package ebib
    :ensure t)
(use-package s
    :ensure t)
(use-package f
    :ensure t)
(use-package hydra
    :ensure t)
(use-package key-chord
    :ensure t)

(require 'org-ref)


