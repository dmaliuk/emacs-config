(require 'projectile)
(require 'helm-projectile)

;; turn on in all modes (check it)
(projectile-global-mode)

;; set completion system
(setq projectile-completion-system 'helm)

;; active helm-projection key bindings
(helm-projectile-on)

;; to limit the number of sources for "helm-projectile" command
(setq helm-projectile-sources-list '(helm-source-projectile-projects
                                     helm-source-projectile-files-list))

;; set the default action on "switch to project" (C-c p p or C-c p h)
;; (setq projectile-switch-project-action 'projectile-find-file)
 ;; even better, list of projects is always on top and the list of other sources is project specific
(setq projectile-switch-project-action 'helm-projectile)



;; use caching to expedite reindexing
(setq projectile-enable-caching t)



(provide 'setup-projectile)
