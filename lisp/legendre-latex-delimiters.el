(require 'smartparens)
(require 'hydra)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The point of this plugin is to setup a hydra keymap so that we can     ;;
;; easily change the delimiters in LaTeX plugins. For example, perhaps we ;;
;; want to turn, e.g., $(\sum_i a_i|)$ into $\left(\sum a_i|\right)$,     ;;
;; where `|` denotes where the cursor is.                                 ;;
;;                                                                        ;;
;; The main hydra, when invoked, will change the innermost pair           ;;
;; surrounding the cursor. Here is a rough description:                   ;;
;;             * Suppose a delimiter is pressed. Then both the            ;;
;;               left and right delimiters are changed to that            ;;
;;               delimiter.                                               ;;
;;               * Valid delimiters include:                              ;;
;;                   `\{`, `[`, `(`, `|`, `<`                             ;;
;;               * The corresponding size of the delimiter is             ;;
;;                 preserved (e.g. if the current delimiter is            ;;
;;                 biggr() then after pressing `{` the new delimiters     ;;
;;                 will be changed to biggr{}).                           ;;
;;                                                                        ;;
;;             * Before a delimiter is pressed, one can press             ;;
;;               one of the keys {1,2,3}. The effect of this is to modify ;;
;;               the new delimiter to "bigl/r", "biggl/r", and "Biglr"    ;;
;;               * Since the most common one (in my usage)                ;;
;;                 is biggl/r, one can also press g to stand for 2.       ;;
;;               * Use `l` to turn left/right pair.                       ;;
;;                                                                        ;;
;;             * size modifications: press + or - to cycle between        ;;
;;               changing sizes of the current delimiter. Note that this  ;;
;;               does not change the current delimiter.                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar my/latex-delimiter-sizes '(normal big bigg Big Bigg))
(defvar my/latex-delimiters '("(" ")" "[" "]" "{" "}" "\\langle" "\\rangle"))

(defun my/latex-get-current-delimiter ()
    (sp-get (sp-get-enclosing-sexp) :op))

(defun my/latex-get-current-delimiter-size ()
  (sp-get (sp-get-enclosing-sexp) :op))

