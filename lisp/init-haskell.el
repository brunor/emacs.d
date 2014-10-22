(require-package 'haskell-mode)
(require-package 'company-ghc)

(when (> emacs-major-version 23)
  (require-package 'flycheck-hdevtools)
  (require-package 'flycheck-haskell)
  (after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

    (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
      "Don't run stylish-buffer if the buffer appears to have a syntax error."
      (unless (flycheck-has-current-errors-p 'error)
        ad-do-it))))


(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(after-load 'haskell-interactive-mode
  (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(require-package 'hi2)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-hook 'haskell-mode-hook (lambda ()
                               (company-mode)
                               (ghc-init)
                               (turn-on-haskell-indentation)
                               (interactive-haskell-mode)
                               (turn-on-hi2)
                               (subword-mode +1)
                               (add-to-list 'company-backends 'company-ghc)
                               (custom-set-variables '(company-ghc-show-info t))))



(setq-default haskell-stylish-on-save t)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line)
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(custom-set-variables '(haskell-process-type 'cabal-repl))
(custom-set-variables '(haskell-tags-on-save t))

(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

(eval-after-load 'page-break-lines
  '(push 'haskell-mode page-break-lines-modes))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))


;; interactive commands
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))


(provide 'init-haskell)
