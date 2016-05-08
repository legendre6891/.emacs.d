(use-package s
    :ensure t)

(require 'cdlatex)
(require 'smartparens)
(require 'key-combo)

;;; ---------------------------------------------------------------------------
;;; 	This is Kevin Li's LaTeX configuration, meant to complement
;;; the configuration that is provided in the main init.el file.
;;; Maybe someday, I'll move everything into one package, but for
;;; today I'll just focus on adding new functionality first.
;;; ---------------------------------------------------------------------------


;;; ---------------------------------------------------------------------------
;;; It is useful to insert environments without resorting to tab,
;;; so let's set that up.
;;; ---------------------------------------------------------------------------
(defun ll/thing-at-line ()
  (let (p1 p2 myLine)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (buffer-substring-no-properties p1 p2)))

(defun ll/delete-at-line ()
  (delete-region (line-beginning-position)
                 (line-end-position)))

(defun ll/line-is (text)
  (and (= (current-column) (length text))
       (string= (ll/thing-at-line) text)))

(defun ll/conditional-insert-environment (text env)
  (if (ll/line-is text)
      (progn
        (ll/delete-at-line)
        (cdlatex-environment env))))

(defun ll/conditional-insert-environment2 (text env1 env2)
  (if (ll/line-is text)
      (progn
        (ll/delete-at-line)
        (cdlatex-environment env1)
        (cdlatex-environment env2))))

;;; ----------------------------------------------------------------------------
;;; Now the automatic key stuffs

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "e")
    (lambda ()
      (interactive)
      (insert "e")
      (ll/conditional-insert-environment "/e" "equation")
      ))

  (define-key LaTeX-mode-map (kbd "p")
    (lambda ()
      (interactive)
      (insert "p")
      (ll/conditional-insert-environment2 "pp" "equation" "split")
      ))

  (define-key LaTeX-mode-map (kbd "l")
    (lambda ()
      (interactive)
      (insert "l")
      (ll/conditional-insert-environment "ll" "align")
      ))

  (define-key LaTeX-mode-map (kbd "d")
    (lambda ()
      (interactive)
      (insert "d")
      (ll/conditional-insert-environment "/d" "description")
      ))

  (define-key LaTeX-mode-map (kbd "i")
    (lambda ()
      (interactive)
      (insert "i")
      (ll/conditional-insert-environment "/i" "itemize")
      ))

  (define-key LaTeX-mode-map (kbd "n")
    (lambda ()
      (interactive)
      (insert "n")
      (ll/conditional-insert-environment "/n" "enumerate")
      ))

  (key-combo-define LaTeX-mode-map "="  '("= " "eq " "equal "))

  )
;;; ----------------------------------------------------------------------------


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


(provide 'legendre-latex-keys)
