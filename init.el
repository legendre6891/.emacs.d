(package-initialize) ;; for org mode
(require 'org)

;; Check version of org before loading our file.
(let ((version org-version))
  (when (version< org-version "8.3.1")
    (message-box "This emacs configuration requires
 org version >= 8.3.1. Please run \"emacs -Q\" and 
 install the latest version of Org mode.")))


(org-babel-load-file
 (expand-file-name
  "my-init.org"
  (concat user-emacs-directory "org")))


;; (add-to-list 'load-path "~/.emacs.d/org-ref/")
;; (use-package dash
;;     :ensure t)

;; (use-package helm-bibtex
;;     :ensure t)

;; (use-package ebib
;;     :ensure t)
;; (use-package s
;;     :ensure t)
;; (use-package f
;;     :ensure t)
;; (use-package key-chord
;;     :ensure t)

;; (require 'org-ref)
