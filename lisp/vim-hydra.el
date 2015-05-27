(use-package hydra
    :ensure t)

(defhydra hydra-change-mode (:color blue
                             :body-pre (insert "j")
                             :idle 1.0)
  ("k" (progn
         (delete-char -1)
         (evil-normal-state))))

(define-key evil-insert-state-map (kbd "j") 'hydra-change-mode/body)


(provide 'vim-hydra)
