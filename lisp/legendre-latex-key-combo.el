;;; -*- lexical-binding: t -*-

(use-package s
    :ensure t)
(use-package key-combo
    :ensure t)


;;------------------------------------------------------------------------
;; The general idea is to bind a key to a list
;; '((cond1 . cmd1)
;;   (cond2 . cmd2)
;;   (cond3 . cmd3)
;;   ...
;;  )
;;
;; The behavior of such a binding is that cmd_i is executed,
;; where i is the smallest index for which cond_i is true.
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;; We first define a function that takes in such a list
;; and returns a function `f` such that calling f has the same
;; effect as executing the command.
;;------------------------------------------------------------------------
;;    in the following functions `X` is the list of the shape above
;;------------------------------------------------------------------------


(defun legendre/func-list-2-cmd (X)
  (lexical-let ((result (--first (eval (car it)) X)))
    (if result
        (cdr result)
        'ignore)))

(defun legendre/func-list-2-func (X)
  (lambda ()
    (interactive)
    (call-interactively (legendre/func-list-2-cmd X))))


(defun legendre/deep-map (f xs)
  (if (not (null xs))
      (let ((x (first xs))
            (stuff (deep-map f (rest xs))))
        (if (atom x)
            (cons (funcall f x) stuff)
            (cons (deep-map f x) stuff))
        )
      nil
      ))

(defun legendre/replacement (x y)
  (lambda (z)
    (if (eq z x)
        y
        z)))