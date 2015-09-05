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

(defun toggle-frame-split ()
    "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
    (interactive)
    (unless (= (length (window-list)) 2)
      (error "Can only toggle a frame split in two"))
    (let ((split-vertically-p (window-combined-p)))
      (delete-window) ; closes current window
      (if split-vertically-p
          (split-window-horizontally)
              (split-window-vertically)) ; gives us a split with the
                                        ; other window twice
          (switch-to-buffer nil))) ; restore the original window in
                                        ; this part of the frame

(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 0)
          (num-windows (count-windows)))
      (while  (< i (- num-windows 1))
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (% (+ i 1) num-windows)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))


(global-set-key (kbd "C-x 6") 'toggle-frame-split)
(global-set-key (kbd "C-x 7") 'rotate-windows)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region-or-line)
