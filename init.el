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

(use-package better-defaults
  :ensure t
  :config
  (progn
    (setq ring-bell-function 'ignore)
    (setq inhibit-startup-message t)
    (setq ido-enable-flex-matching t)
    (setq ido-use-virtual-buffers t)
    (setq ido-auto-merge-work-directories-length -1)))

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (progn
    (setq recentf-max-menu-items 500)))


(use-package evil
  :ensure t
  :commands evil-mode
  :init (evil-mode 1))


(use-package smex
  :ensure t
  :bind ("M-x" . smex))

