(require 'org)
(org-babel-load-file
 (expand-file-name
  "my-init.org"
  (concat user-emacs-directory "org"
   )))
