(require 'cdlatex)
(require 'smartparens)
(require 'key-combo)
(require 's)
(require 'dash)

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

;; The following function returns
;; * ('begin . env)
;; * ('end . env)
;; * nil
;; depending on whether the text it receives is of the shape
;; \begin{...}, \end{...} or not.

;; It strips away whitespace
;; and the beginning and end of the line.
(defun ll/begin-end-p (text)
  (let ((text (s-trim text))
        (begin-regex "^\\\\begin{\\([[:ascii:]]+\\)}")
        (end-regex "^\\\\end{\\([[:ascii:]]+\\)}"))
    (if (string-match begin-regex text)
        (cons 'begin (match-string 1 text))
        (if (string-match end-regex text)
            (cons 'end (match-string 1 text))
            nil))))

(defun ll/begin-p (text)
  (let ((result (ll/begin-end-p text)))
    (eq (car result) 'begin)))

(defun ll/end-p (text)
  (let ((result (ll/begin-end-p text)))
    (eq (car result) 'end)))


(defun ll/find-previous-begin ()
  (setq prev 0)
  (setq next 0)
  
  (save-excursion
    (while (and (not (ll/begin-p (ll/thing-at-line)))
                (not (bobp)))
      (setq prev (- prev 1))
      (forward-line -1)))

  (save-excursion
    (while (and (not (ll/end-p (ll/thing-at-line)))
                (not (eobp)))
      (setq next (+ next 1))
      (forward-line 1)))

  (cons prev next))

