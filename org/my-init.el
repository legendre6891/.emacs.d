
;; so we don't get bothered about coding system
(prefer-coding-system 'utf-8) 

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

(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))

(load custom-file)

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

(ensure-in-custom 'user-full-name "Enter your full name: ")
(ensure-in-custom 'email-address "Enter your email address: ")

(setq user-home-folder
      (file-name-as-directory
       ;; Windows doesn't use the ~ convention
       (if (eq system-type 'windows-nt)
           (substitute-in-file-name "$HOMEDRIVE$HOMEPATH")
           "~")))

(setq user-dropbox-folder
      (file-name-as-directory
       (concat user-home-folder "Dropbox")))

(use-package better-defaults
    :ensure t)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq set-mark-command-repeat-pop t)

(setq gc-cons-threshold (* 1024 1024 50))

(setq ido-use-virtual-buffers t)

(setq ido-auto-merge-work-directories-length -1)

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

(ido-everywhere)
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

(load-theme 'aurora t)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(use-package smart-mode-line
    :ensure smart-mode-line
    :init
    (progn
      (setq sml/theme 'light)
      (setq sml/name-width 40)
      (setq sml/mode-width 'full))
    :config
    (progn
      (sml/setup)))

(use-package nlinum
    :ensure t
    :init
    (global-nlinum-mode t))

(use-package yasnippet
    :ensure t
    :commands (yas-expand yas-new-snippet yas-visit-snippet-file)
    :config
    (progn
      (yas-global-mode)
      (define-key yas-minor-mode-map (kbd "<tab>") nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      (define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)))

(use-package smartparens-config
    :ensure smartparens
    :init
    :config
    (progn
      (smartparens-global-mode)
      (add-hook 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
      ;; Some LaTeX specific smartparens
      ))

(setq org-src-fontify-natively t)

(setq org-fontify-whole-heading-line t)

(setq org-list-allow-alphabetical t)

(setq org-catch-invisible-edits 'smart)

(let ((org-folder
       (file-name-as-directory (concat user-dropbox-folder "ORG"))))
  (setq org-directory org-folder))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(org-defkey org-mode-map (kbd "C-.") 'org-preview-latex-fragment)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setq org-latex-listings 'minted)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory -shell-escape %o %f"
       "pdflatex -interaction nonstopmode -output-directory -shell-escape %o %f"
       "pdflatex -interaction nonstopmode -output-directory -shell-escape %o %f"))

(setq lisp-indent-function 'common-lisp-indent-function)
