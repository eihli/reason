(defun run-make ()
  (interactive)
  (shell-command "make"))

(map! :leader :desc "make" "C-o" #'run-make)
