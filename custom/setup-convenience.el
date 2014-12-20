(provide 'setup-convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move point back to indentation of beginning of line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prelude-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


;; update any change made on file to the current buffer
(global-auto-revert-mode)

;; better (more sources) than dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand

;; global line highlighting mode
(global-hl-line-mode)

;; always display ibuffer in another window
(setq ibuffer-use-other-window t)

;; enable line numbers in prog modes only
;; (add-hook 'prog-mode-hook 'linum-mode)

;; navigation between windows: shift + left/right/up/down
(windmove-default-keybindings)
