(require 's)


(defun legendre-latex-write-message ()
  (interactive)
  (message "%s" (append legendre-latex-binary-relations-alist-default legendre-latex-binary-relations-alist))
  (message "%s" legendre-latex-binary-relations-regexps))


(defconst legendre-latex-binary-relations-alist-default
  '("="
    "\\succeq"
    "\\preceq"
    "\\geq"
    "\\leq"
    ">"
    "<"
    "\\in"
    "\\ni"
    "\\succ"
    "\\prec"
    "\\coloneqq"))

(defvar legendre-latex-binary-relations-alist '())


(defun legendre-latex-binary-relations-regexps () 
  (concat
   "\\("
   (regexp-opt-group
    (append
     legendre-latex-binary-relations-alist-default
     legendre-latex-binary-relations-alist))
   "\\).*\\'")
  )


(defun legendre-latex-add-ampersands (astring)
  (interactive)
  (replace-regexp-in-string (legendre-latex-binary-relations-regexps)
                            "&\\&"
                            astring
                            nil nil 1))

(defun legendre-latex-append-backslashes (astring)
  (interactive)
  (concat astring "\\\\"))


(defun legendre-latex-add-alignment (astring)
  (legendre-latex-append-backslashes
   (legendre-latex-add-ampersands astring)))

(defun apply-function-to-region (fn)   
  (interactive "aFunction to apply to region: ")   
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (resulting-text 
            (funcall 
             fn 
             (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (insert resulting-text))))

(defun legendre-latex-align-region (beg end)
  (interactive "r")
  (save-excursion
    (let* ((buffer-string (buffer-substring-no-properties beg end))
           (buffer-lines (s-lines buffer-string))
           (resulting-text
            (s-join "\n"
             (mapcar 'legendre-latex-add-alignment buffer-lines)))
           )
      (kill-region beg end)
      (insert resulting-text))
  ))


(defun legendre-latex-test (astring)
  (s-join "\n"
   (mapcar 'legendre-latex-add-alignment (s-lines astring))))


(provide 'legendre-latex)
