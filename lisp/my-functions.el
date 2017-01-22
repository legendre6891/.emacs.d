(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
            ((vc-backend filename) (vc-rename-file filename new-name))
            (t
             (rename-file filename new-name t)
             (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c C-r") 'rename-file-and-buffer)


(defun comment-auto-fill ()
  (interactive)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(defun quit-or-smex ()
  (interactive)
  (if (window-minibuffer-p (frame-selected-window))
      (keyboard-escape-quit)
    (smex)))


(provide 'my-functions)



