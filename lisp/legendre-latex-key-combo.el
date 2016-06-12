;;; -*- lexical-binding: t -*-
(use-package s
    :ensure t)
(use-package key-combo
    :ensure t)
(use-package ht
    :ensure t)
(require 'texmathp)
(require 'cdlatex)

;;-----------------------------------------------------------------------;;
;;                          GENERAL DESCRIPTION                          ;;
;;-----------------------------------------------------------------------;;
;; The general idea is to bind a key to a list                           ;;
;; '((cond1 . cmd1)                                                      ;;
;;   (cond2 . cmd2)                                                      ;;
;;   (cond3 . cmd3)                                                      ;;
;;   ...                                                                 ;;
;;  )                                                                    ;;
;;                                                                       ;;
;; The behavior of such a binding is that cmd_i is executed,             ;;
;; where i is the smallest index for which cond_i is true.               ;;
;;-----------------------------------------------------------------------;;

;;------------------------------------------------------------------------
;; We first define a function that takes in such a list
;; and returns a function `f` such that calling f has the same
;; effect as executing the command.
;;------------------------------------------------------------------------
;;    in the following functions `X` is the list of the shape above
;;------------------------------------------------------------------------
(defun legendre/func-list-2-cmd (X)
  ;; Take a list of the shape above, and evaluate (in turn) each
  ;; cond1, ..., condn, until cond_i is true. When this is true, return
  ;; cmd_i. If no cond_i is true, then return the function symbol #'ignore
  (lexical-let ((result (--first (eval (car it)) X)))
    (if result (cdr result)
        '(ignore))))

(defun legendre/func-list-2-func (X)
  ;; Take a list of the shape above, and returns a function f.
  ;; When f is called, the function legendre/func-list-2-cmd
  ;; is called (note that conditions are evaulated each time)
  (lambda ()
    (interactive)
    (eval (legendre/func-list-2-cmd X))))


;;----------------------------------------------------------------------
;; Context sensitive functions
;;----------------------------------------------------------------------
;; The general idea here is that in the any condition/command list as
;; above, we would like to replace some symbols found in
;; cond1, ..., condn, cmd1, ..., cmdn with symbols relevant in the
;; environment.

;; For example, I would like to have *before* be replaced with
;; (preceding-char), and *after* be replaced with (following-char), etc.

;; We define the following functions here for easy access. when called,
;; they should return a value of some sort (function or value).
;;----------------------------------------------------------------------
(defun legendre/point-after-space? ()
  (eq (preceding-char) ? ))

(defun legendre/point-after-closing-brace? ()
  (eq (preceding-char) ?\}))

(defun legendre/point-after-digit? ()
  (let ((pc (preceding-char)))
    (and (<= ?0 pc)
         (>= ?9 pc))))
(defun legendre/point-after-letter? ()
  (let ((pc (preceding-char)))
    (or (and (<= ?a pc)
             (>= ?z pc))
        (and (<= ?A pc)
             (>= ?Z pc)))))

(defun legendre/looking-back-string (x)
  ;; Check if right before the cursor, x is the string
  ;; not very robust, but works well enough
  (let ((l (length x)))
    (setq legendre/legendre-key-combo-internal-string-length (length x))
    (string-equal
     x
     (buffer-substring-no-properties (- (point) l) (point)))))

(defun legendre/looking-back-char (chr)
  (setq legendre/legendre-key-combo-internal-string-length 1)
  (equal (preceding-char) chr))


;;----------------------------------------------------------------------
;; Setup the substitution
;;----------------------------------------------------------------------

(defun legendre/deep-map (f xs)
  (if (not (null xs))
      (let ((x (first xs))
            (stuff (legendre/deep-map f (rest xs))))
        (if (atom x)
            (cons (funcall f x) stuff)
            (cons (legendre/deep-map f x) stuff)))
      nil))

(defun legendre/substitute-function (H)
  ;; Here H is a hash table created with (ht-create).
  ;; This function returns a function f such that
  ;; f(x) = x if x is not in H. Otherwise, it returns H(x).
  #'(lambda (x)
      (if (ht-contains? H x)
          (ht-get H x)
          x)))


(setq legendre/legendre-key-combo-internal-string-length 0)

(defun legendre/looking-back-string (x)
  ;; Check if right before the cursor, x is the string
  ;; not very robust, but works well enough
  (let ((l (length x)))
    (setq legendre/legendre-key-combo-internal-string-length (length x))
    (string-equal
     x
     (buffer-substring-no-properties (- (point) l) (point)))))


(setq legendre/replacement-hash (ht<-alist
       '((*before*               . (preceding-char))
         (*before-is*            . (lambda (x) (legendre/looking-back-char x)))
         (*Before-Is*            . (lambda (x) (legendre/looking-back-string x)))
         (*match-length*         . legendre/legendre-key-combo-internal-string-length)
         (*after*                . (following-char))
         (*after-space?*         . (legendre/point-after-space?))
         (*after-digit?*         . (legendre/point-after-digit?))
         (*after-letter?*        . (legendre/point-after-letter?))
         (*after-closing-brace?* . (legendre/point-after-closing-brace?))
         (*delete*               . (delete-backward-char legendre/legendre-key-combo-internal-string-length))
         (*delete1*              . (delete-backward-char 1))
         (*math*                 . (texmathp))
         (*no-math*              . (not (texmathp)))
         (*default-insert*       . (self-insert-command 1))
         )))



;;----------------------------------------------------------------------
;; Final Utility Functions
;;----------------------------------------------------------------------
(defun make-smart-function (X)
  ;; From a command list X (see the very beginning of this file)
  ;; make a function. This function should be bound to a key.
  ;;
  ;; See the examples below
  (legendre/func-list-2-func
   (legendre/deep-map
    (legendre/substitute-function legendre/replacement-hash)
    X)))

;; (setq example-list
;;       '((*after-space?* . (insert "3"))
;;         (t . (insert " "))))

;; (global-set-key (kbd "<f11>") (make-smart-function example-list))

;;----------------------------------------------------------------------
;; Load stuff
;;----------------------------------------------------------------------
(setq space-list
      '((*no-math*              . (progn *default-insert*))
        ((*Before-Is* "<==>")   . (progn *delete* (insert "\\iff ")))
        ((*Before-Is* ">=")     . (progn *delete* (insert "\\geq ")))
        ((*Before-Is* "<=")     . (progn *delete* (insert "\\leq ")))
        ((*Before-Is* "==>")    . (progn *delete* (insert "\\implies ")))
        ((*Before-Is* "<==")    . (progn *delete* (insert "\\impliedby ")))
        ((*Before-Is* "==")     . (progn *delete* (insert "&= ")))
        ((*Before-Is* "+-")     . (progn *delete* (insert "\\pm ")))
        ((*Before-Is* "|-")     . (progn *delete* (insert "\\perp ")))
        ((*Before-Is* "dist=>") . (progn *delete* (insert "\\overset{\\text{dist}}{\\Longrightarrow} ")))
        ((*Before-Is* "ae=>")   . (progn *delete* (insert "\\overset{\\text{a.e.}}{\\Longrightarrow} ")))
        ((*Before-Is* "P=>")    . (progn *delete* (insert "\\overset{\\P}{\\Longrightarrow} ")))
        ((*Before-Is* "N(0,1)") . (progn *delete* (insert "\\mathcal{N}(0,1)")))
        (t                      . (progn *default-insert*))))

(defun legendre/digit-list (digit)
  `((*no-math*              . (progn *default-insert*))
    (*after-letter?*        . (insert (format "_{%c}" ,digit)))
    (*after-closing-brace?* . (save-excursion (backward-char 1) (insert (format "%c" ,digit))))
    (t                      . (progn *default-insert*))))

(setq zero-list  (legendre/digit-list ?0))
(setq one-list   (legendre/digit-list ?1))
(setq two-list   (legendre/digit-list ?2))
(setq three-list (legendre/digit-list ?3))
(setq four-list  (legendre/digit-list ?4))
(setq five-list  (legendre/digit-list ?5))
(setq six-list   (legendre/digit-list ?6))
(setq seven-list (legendre/digit-list ?7))
(setq eight-list (legendre/digit-list ?8))
(setq nine-list  (legendre/digit-list ?9))

(setq dot-list
      '(((*Before-Is* "..") . (progn *delete* (insert "\\dots")))
        (t . (progn *default-insert*))))

(setq caret-list
      '((*no-math*        . (cdlatex-sub-superscript))
        ((*before-is* ? ) . (insert-then-position "\\what{?}"))
        (t                . (cdlatex-sub-superscript))))

(setq tilde-list
      '((*no-math*         . (progn *default-insert*))
        (*math*            . (insert-then-position "\\wtilde{?}"))))



(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "SPC") (make-smart-function space-list))
  (define-key LaTeX-mode-map (kbd "0") (make-smart-function zero-list))
  (define-key LaTeX-mode-map (kbd "1") (make-smart-function one-list))
  (define-key LaTeX-mode-map (kbd "2") (make-smart-function two-list))
  (define-key LaTeX-mode-map (kbd "3") (make-smart-function three-list))
  (define-key LaTeX-mode-map (kbd "4") (make-smart-function four-list))
  (define-key LaTeX-mode-map (kbd "5") (make-smart-function five-list))
  (define-key LaTeX-mode-map (kbd "6") (make-smart-function six-list))
  (define-key LaTeX-mode-map (kbd "7") (make-smart-function seven-list))
  (define-key LaTeX-mode-map (kbd "8") (make-smart-function eight-list))
  (define-key LaTeX-mode-map (kbd "9") (make-smart-function nine-list))
  (define-key LaTeX-mode-map (kbd ".") (make-smart-function dot-list))
  (define-key LaTeX-mode-map (kbd "~") (make-smart-function tilde-list))
  (define-key LaTeX-mode-map (kbd "^") (make-smart-function caret-list))
  )

;;----------------------------------------------------------------------
(provide 'legendre-latex-key-combo)