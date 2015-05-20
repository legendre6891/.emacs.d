(use-package hydra
  :ensure t)

(defhydra hydra-change-mode ()
  "change evil mode"
  ("k" evil-normal-state "change to normal state"))

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-circle-symbol (:color blue)
  "string"
  ("+" (insert "\\oplus") "⊕")
  ("@" (insert "\\infty") "∞")
  ("TAB" hydra-square-symbol/body "a")
  )

(defhydra hydra-square-symbol (:color blue)
  "string"
  ("+" (insert "\\otimes") "⊕")
  ("@" (insert "\\infty") "∞")
  )

(define-key evil-insert-state-map (kbd "<f2>") 'hydra-zoom/body)
;; (define-key evil-insert-state-map (kbd "j") 'hydra-change-mode/body)
(evil-define-key 'insert LaTeX-mode-map (kbd "@") 'hydra-circle-symbol/body)



