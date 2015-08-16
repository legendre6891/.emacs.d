;; ======================================================================
;; latex!!

(use-package s
    :ensure t)

(use-package reftex ; TeX/BibTeX cross-reference management
    :defer t
    :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
    :config
    (progn
      ;; Plug into AUCTeX
      (setq reftex-plug-into-AUCTeX t
            reftex-label-alist
            '(("axiom" ?a "ax:" "~\\ref{%s}" t ("axiom" "ax.") -2)
              ("theorem" ?h "thr:" "~\\ref{%s}" t ("theorem" "th.") -3)
              ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "le.") -3)
              ("claim" ?m "clm:" "~\\ref{%s}" t ("claim" "cl.") -3)
              ("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition" "pr.") -3)
              ("wts" ?w "wts:" "~\\ref{%s}" t ("wts" "wt.") -3)
              ("definition" ?d "defn:" "~\\ref{%s}" t ("definition" "de.") -3)))
      ;; Provide basic RefTeX support for biblatex
      (unless (assq 'biblatex reftex-cite-format-builtin)
        (add-to-list 'reftex-cite-format-builtin
                     '(biblatex "The biblatex package"
                       ((?\C-m . "\\cite[]{%l}")
                        (?t . "\\textcite{%l}")
                        (?a . "\\autocite[]{%l}")
                        (?p . "\\parencite{%l}")
                        (?f . "\\footcite[][]{%l}")
                        (?F . "\\fullcite[]{%l}")
                        (?x . "[]{%l}")
                        (?X . "{%l}"))))
        (setq reftex-cite-format 'biblatex)))
    :diminish reftex-mode)

(use-package auctex
    :ensure t
    :commands (latex-mode LaTeX-mode plain-tex-mode LaTeX-mode-map)
    :init
    (progn
      (setq TeX-parse-self t)			; Enable parse on load.
      (setq TeX-auto-save t)			; Enable parse on save.
      (setq TeX-PDF-mode t)
      (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
      (add-hook 'LaTeX-mode-hook '(lambda () (fci-mode 1)))
      (add-hook 'LaTeX-mode-hook '(lambda () (electric-indent-mode -1)))
      (add-hook 'LaTeX-mode-hook
                (lambda ()
                  (LaTeX-add-environments
                   '("axiom" LaTeX-env-label)
                   '("theorem" LaTeX-env-label)
                   '("lemma" LaTeX-env-label)
                   '("claim" LaTeX-env-label)
                   '("proposition" LaTeX-env-label)
                   '("wts" LaTeX-env-label)
                   '("definition" LaTeX-env-label)
                   )))
      (setq TeX-output-view-style '("^pdf$" "." "SumatraPDF.exe -reuse-instance %o"))
      (setq TeX-view-program-list
            '(("SumatraPDF" "SumatraPDF.exe -reuse-instance %o")
              ))
      (cond
        ((eq system-type 'windows-nt)
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
                                                        (output-dvi "Yap"))))))
        ((eq system-type 'gnu/linux)
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (setq TeX-view-program-selection '((output-pdf "evince")
                                                        (output-dvi "evince")))))))))

(if (eq system-type 'darwin)
    (setq
     ;; Set the list of viewers for Mac OS X.
     TeX-view-program-list
     '(("Preview.app" "open -a Preview.app %o")
       ("Skim" "open -a Skim.app %o")
       ("displayline" "displayline %n %o %b")
       ("open" "open %o"))
     ;; Select the viewers for each file type.
     TeX-view-program-selection
     '((output-dvi "open")
       (output-pdf "Skim")
       (output-html "open"))))

(use-package cdlatex
    :ensure t
    :config
    (progn
      (add-hook 'TeX-mode-hook 'turn-on-cdlatex)
      (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
      (setq cdlatex-paired-parens "$[{(")
      (setq cdlatex-command-alist
            '(("bi" "Insert \\binom{}{}" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
              ("ggr(" "Insert \biggl( \biggr)" "\\biggl(? \\biggr" cdlatex-position-cursor nil nil t)
              ("ggr|" "Insert \biggl| \biggr|" "\\biggl|? \\biggr|" cdlatex-position-cursor nil nil t)
              ("ggr{" "Insert \biggl\{ \biggr\}" "\\biggl\\{? \\biggr\\" cdlatex-position-cursor nil nil t)
              ("ggr[" "Insert \biggl[ \biggr]" "\\biggl[? \\biggr" cdlatex-position-cursor nil nil t)
              ("ce" "Insert ceilings" "\\lceil? \\rceil" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\lfloor? \\rfloor" cdlatex-position-cursor nil nil t)
              ("ggrce" "Insert ceilings" "\\biggl\\lceil? \\biggr\\rceil" cdlatex-position-cursor nil nil t)
              ("int" "Insert integrals without limits" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
              ("sum" "Insert sums without limits" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prod" "Insert products without limits" "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
              ("prodl" "Insert products" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
              ("fl" "Insert floors" "\\biggl\\lfloor? \\biggr\\rfloor" cdlatex-position-cursor nil nil t)
              ("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
              ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
              ("lem" "Insert lemma env" "" cdlatex-environment ("lemma") t nil)
              ("clm" "Insert claim env" "" cdlatex-environment ("claim") t nil)
              ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
              ("wts" "Insert want to show env" "" cdlatex-environment ("wts") t nil)
              ("def" "Insert definition env" "" cdlatex-environment ("definition") t nil)
              ("pr" "Insert proof env" "" cdlatex-environment ("proof") t nil)))
      (setq cdlatex-math-modify-alist
            '((?t "\\text" nil t nil nil)
              (?s "\\mathscr" nil t nil nil)))
      (setq cdlatex-env-alist
            '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
              ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
              ("lemma" "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
              ("claim" "\\begin{claim}\nAUTOLABEL\n?\n\\end{claim}\n" nil)
              ("proposition" "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
              ("wts" "\\begin{wts}\nAUTOLABEL\n?\n\\end{wts}\n" nil)
              ("definition" "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil))))
    :diminish cdlatex-mode)
