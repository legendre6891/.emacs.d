;;; See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

;;; The first thing to make emacs load faster
(setq gc-cons-threshold (* 1024 1024 50))

(let ((file-name-handler-alist nil))


(package-initialize) ;; for org mode
(require 'org)

;; Check version of org before loading our file.
(let ((version org-version))
  (when (version< org-version "8.3.1")
    (message-box "This emacs configuration requires
Org version >= 8.3.1. Please run \"emacs -Q\" and
nstall the latest version of Org mode.")))


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
)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
