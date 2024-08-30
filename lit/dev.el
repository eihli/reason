(defun run-make ()
  (shell-command "make"))

(global-set-key (kbd "C-o") 'run-make)
