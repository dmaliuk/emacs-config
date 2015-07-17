(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(elpy-enable)
(elpy-use-ipython)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(magit
    jedi
    auto-complete
    autopair
    company
    ggtags
    helm
    helm-gtags
    function-args
    clean-aindent-mode
    dtrt-indent
    ws-butler
    yasnippet
    smartparens
    projectile
    helm-projectile
    expand-region
    undo-tree
    ack-and-a-half
    volatile-highlights
    sr-speedbar
    workgroups2
    elpy
    virtualenvwrapper
    markdown-mode))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-convenience)
(require 'setup-projectile)
(require 'setup-virtualenvwrapper)
(require 'setup-ipython)

;; proprly handle environment vars
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; this is necessary to initialize c-mode-map and c-mode-base-map
(require 'cc-mode)

;; load sr-speedbar
(require 'sr-speedbar)

;; function-args
;;(require 'function-args)
;;(fa-config-default)
;;(define-key c-mode-map  [(tab)] 'moo-complete)
;;(define-key c++-mode-map  [(tab)] 'moo-complete)

;; map indent as TAB
(define-key c-mode-map  [(tab)] 'c-indent-command)
(define-key c++-mode-map  [(tab)] 'c-indent-command)

;; company: shows hints as you type from various sources
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; leave only relevant back-end: still not sure what each means, but files is for path completion
(setq company-backends '(company-files company-c-headers company-clang company-ropemacs company-dabbrev-code company-gtags))

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)
(define-key c-mode-base-map (kbd "C-<tab>") 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Wind-move
(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-l") 'windmove-up)
(global-set-key (kbd "C-c C-;") 'windmove-right)

;; convenience mappings
(global-set-key (kbd "C--") 'previous-buffer)
(global-set-key (kbd "C-=") 'next-buffer)

;; enable gdb mode
(setq gdb-many-windows t)
(setq gdb-show-main t)

;; yasnippet mode
(require 'yasnippet)
(yas-global-mode 1)

;; enable clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)

;; load sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; enable volatile-highlights mode
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; activate ess
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python related stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

;;(require 'auto-complete)
;;(require 'autopair)
;;(require 'flycheck)
;;(global-flycheck-mode t)

;; auto-complete mode extras
;; (setq
;;  ac-auto-start 2
;;  ac-override-local-map nil
;;  ac-use-menu-map t
;;  ac-candidate-limit 20)

;; python
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;; (setq py-electric-colon-active t)
;; (add-hook 'python-mode-hook 'autopair-mode)
;; (add-hook 'python-mode-hook 'yas-minor-mode)

;; jedi: auto-completion
;; (require 'jedi)
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (jedi:setup)
;; 	    (jedi:ac-setup)
;;             (local-set-key "\C-cd" 'jedi:show-doc)
;;             (local-set-key (kbd "M-SPC") 'jedi:complete)
;;             (local-set-key (kbd "M-.") 'jedi:goto-definition)))

;; (add-hook 'python-mode-hook 'auto-complete-mode)

;; (show-paren-mode t)

;; disable automatic backup
(setq make-backup-files nil)

;; disble scroll-bar in every window
(scroll-bar-mode -1)

;; custom settings: allow moving windows Super-shift-up/down/left/right
(require 'buffer-move)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; protobuf-mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.prototxt\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; ispc code c-mode
(add-to-list 'auto-mode-alist '("\\.ispc\\'" . c-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change default c/c++ indentation style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-default-style "linux")
(setq c-basic-offset 4)
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; show unncessary whitespace that can mess up your diff
;;(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; add CUDA extention to c-mode alist
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; add cmake files to cmak
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode) '("\\.cmake\\'" . cmake-mode))

;; MAC: set META and SUPER
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; set default font and frame size
(set-frame-font "Menlo 15" nil t)
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))

;; remap buffer list to ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; turn beep off
(setq visible-bell nil)

;; turn off toolbar
(tool-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workgroups2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'workgroups2)
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

(message "Ready to play!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#eee8d5")
 '(foreground-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(workgroups-mode 1)
