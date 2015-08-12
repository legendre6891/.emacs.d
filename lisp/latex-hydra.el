;; (use-package hydra
;;     :ensure t)

;; (defhydra hydra-circle-symbol (:body-pre (insert "\\circ")
;;                                :color blue
;;                                :idle 1.0)
;;   "insert symbol with circles"
;;   ("+" (progn
;;          (delete-backward-char 5)
;;          (insert "\\oplus")) "⊕")
;;   ("@" (progn
;;          (delete-backward-char 5)
;;          (insert "\\infty")) "⊕")
;;   ("*" (progn
;;          (delete-backward-char 5)
;;          (insert "\\otimes")) "⊕"))

;; (evil-define-key 'insert LaTeX-mode-map (kbd "@") 'hydra-circle-symbol/body)
;; (evil-define-key 'insert TeX-mode-map (kbd "@") 'hydra-circle-symbol/body)

;; ;;; See http://stackoverflow.com/questions/16119853/elisp-close-async-shell-command-window-after-the-command-finishes
;; (defun latex-sentinel (process event)
;;   (message event)
;;   (cond ((string-match-p "finished" event)
;;          (progn
;;            (kill-buffer "*async arara*")
;;            (message "arara done")))))

;; (defun latex-compile ()
;;   "Runs pdflatex on current file"
;;   (interactive)
;;   (let* ((file-name (shell-quote-argument (buffer-file-name)))
;;          (process (start-process-shell-command
;;                    "arara"
;;                    "*async arara*"
;;                    (concat "arara " file-name))))
;;     (set-process-sentinel process 'latex-sentinel)))

;; (defhydra hydra-latex-compile (:color blue
;;                                :body-pre (insert "K")
;;                                :idle 1.0)
;;   "compile latex source"
;;   ("K" (progn
;;          (delete-char -1)
;;          (save-buffer)
;;          (latex-compile))
;;        "Compile")
;;   ("L" (progn
;;          (delete-char -1)
;;          (TeX-view)) "View"))


;; (evil-define-key 'normal LaTeX-mode-map (kbd "K") 'hydra-latex-compile/body)
;; (evil-define-key 'normal TeX-mode-map (kbd "K") 'hydra-latex-compile/body)

;; (evil-define-key 'insert LaTeX-mode-map (kbd "K") 'hydra-latex-compile/body)
;; (evil-define-key 'insert TeX-mode-map (kbd "K") 'hydra-latex-compile/body)

;; (provide 'latex-hydra)
