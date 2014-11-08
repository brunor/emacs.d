;; (when (< emacs-major-version 24)
;;   (require-package 'color-theme))

(require 'color-theme-solarized)

;; If you don't customize it, this is the theme you get.
;;(setq-default custom-enabled-themes '(solarized-dark))
(load-theme 'solarized-dark t)


(provide 'init-themes)
