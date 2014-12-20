(require 'cc-mode)
(require 'semantic)

;; Semantic is the first package in CEDET
;; Semantic related features
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-stickyfunc-mode 1)
;; enable semantic mode
(semantic-mode 1)

;; EDE is the second package in CEDET
;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

;; enable function args completion
(require 'function-args)
(fa-config-default)
;;(define-key c-mode-map  [(contrl tab)] 'moo-complete)
;;(define-key c++-mode-map  [(control tab)] 'moo-complete)
;;(define-key c-mode-map (kbd "M-o")  'fa-show)
;;(define-key c++-mode-map (kbd "M-o")  'fa-show)

(provide 'setup-cedet)
