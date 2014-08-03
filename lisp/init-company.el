(require-package 'company)

;; backends
(require-package 'company-ghc)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
