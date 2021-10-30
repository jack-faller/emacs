(setq package-enable-at-startup nil)

;; disable gc during startup and set to a higher number after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 100000000)))
