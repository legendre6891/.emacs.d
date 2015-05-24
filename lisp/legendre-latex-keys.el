(defun insert-latex-string (astring)
  (lexical-let ((astring astring))
   (lambda ()
     (interactive)
     (insert astring)
     (cdlatex-position-cursor))))

(eval-after-load 'latex
  '(evil-define-key 'insert LaTeX-mode-map (kbd "C-2")
    (insert-latex-string "\\sqrt{?}")))

(eval-after-load 'latex
  '(evil-define-key 'insert LaTeX-mode-map (kbd "C--")
    (insert-latex-string "\\overline{?}")))

(eval-after-load 'latex
  '(evil-define-key 'insert LaTeX-mode-map (kbd "C-_")
    (insert-latex-string "\\underline{?}")))


(provide 'legendre-latex-keys)
