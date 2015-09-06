;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; ANSI colors in shell mode would be nice by default:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-names-vector ["white" "light red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"])

(provide 'setup-shell)
