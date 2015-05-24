(use-package hydra
    :ensure t)


(defhydra hydra-change-mode (:color blue
                             :body-pre (insert "j")
                             :idle 1.0)
  ("k" (progn
         (delete-char -1)
         (evil-normal-state))))

(define-key evil-insert-state-map (kbd "j") 'hydra-change-mode/body)

(defhydra hydra-circle-symbol (:body-pre (insert "\\circ")
                               :color blue
                               :idle 1.0)
  "insert symbol with circles"
  ("+" (progn
         (delete-backward-char 5)
         (insert "\\oplus")) "⊕")
  ("@" (progn
         (delete-backward-char 5)
         (insert "\\infty")) "⊕")
  ("*" (progn
         (delete-backward-char 5)
         (insert "\\otimes")) "⊕")
  ) 

(evil-define-key 'insert LaTeX-mode-map (kbd "@") 'hydra-circle-symbol/body)




