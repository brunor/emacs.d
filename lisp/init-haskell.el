;;; haskell settings

(require-package 'haskell-mode)
(require-package 'company-ghc)
(require-package 'hi2)
(require-package 'flycheck-hdevtools)
(require-package 'flycheck-haskell)
(require-package 'shm)
(require-package 'ghci-completion)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;;--------------------------------------------------------------
;; functions
;;--------------------------------------------------------------

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))


(defvar haskell-process-use-ghci nil)

(defun haskell-process-cabal-build-and-restart ()
  "Build and restart the Cabal project."
  (interactive)
  (cond
   (haskell-process-use-ghci
    (when (buffer-file-name)
      (save-buffer))
    ;; Reload main module where `main' function is
    (haskell-process-reload-devel-main))
   (t
    (haskell-process-cabal-build)
    (haskell-process-queue-without-filters
     (haskell-process)
     (format ":!cd %s && scripts/restart\n" (haskell-session-cabal-dir (haskell-session)))))
   (t (turbo-devel-reload))))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     ;; Use ag
     (t (ag-files sym
                  "\\.hs$"
                  (haskell-session-current-dir (haskell-session)))))))

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module ")
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)
    (god-mode)))

(defun shm-contextual-space ()
    "Do contextual space first, and run shm/space if no change in
the cursor position happened."
    (interactive)
    (if god-local-mode
        (call-interactively 'god-mode-self-insert)
      (if (looking-back "import")
          (call-interactively 'haskell-mode-contextual-space)
        (progn
          (let ((ident (haskell-ident-at-point)))
            (when ident
              (and interactive-haskell-mode
                   (haskell-process-do-try-type ident))))
          (call-interactively 'shm/space)))))

(defun shm/insert-putstrln ()
  "Insert a putStrLn."
  (interactive)
  (let ((name
         (save-excursion
           (goto-char (car (shm-decl-points)))
           (buffer-substring-no-properties
            (point)
            (1- (search-forward " "))))))
    (insert
     (format "putStrLn \"%s:%s:%d\""
             (file-name-nondirectory (buffer-file-name))
             name
                          (line-number-at-pos)))))


;;-----------------------------------------------------------------------------------
;; mode settings
;;-----------------------------------------------------------------------------------
(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-ghci '())
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(company-ghc-show-info t)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t))


(setq haskell-complete-module-preferred
      '("Data.ByteString"
        "Data.ByteString.Lazy"
        "Data.Conduit"
        "Data.Function"
        "Data.List"
        "Data.Map"
        "Data.Maybe"
        "Data.Monoid"
        "Data.Ord"))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

;; (setq haskell-process-path-ghci "ghci-ng")
;; (setq haskell-process-args-ghci '("-ferror-spans"))
;; (setq haskell-process-args-cabal-repl
;;             '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))

;;-----------------------------------------------------------------------------------
;; add hook
;;-----------------------------------------------------------------------------------
;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

;;(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)


(after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

  (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
    "Don't run stylish-buffer if the buffer appears to have a syntax error."
    (unless (flycheck-has-current-errors-p 'error)
      ad-do-it)))


;; (dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
;;   (add-hook hook 'turn-on-haskell-doc-mode))
;;(Add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (after-load 'haskell-interactive-mode
;;   (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;; (add-hook 'haskell-mode-hook (lambda ()
;;                                (company-mode)
;;                                (ghc-init)
;;                                ;;                               (turn-on-haskell-indentation)
;;                                (structured-haskell-mode)
;;                                (interactive-haskell-mode)
;;                                (turn-on-hi2)
;;                                (subword-mode +1)
;;                                (custom-set-variables '(company-ghc-show-info t))))

(eval-after-load 'company '(add-to-list 'company-backends 'company-ghc))

;; (after-load 'haskell-mode
;;   (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
;;   (define-key haskell-mode-map (kbd "C-o") 'open-line)
;;   (define-key haskell-mode-map [f8] 'haskell-navigate-imports))


;; (when (eval-when-compile (>= emacs-major-version 24))
;;   (require-package 'ghci-completion)
;;   (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; (eval-after-load 'page-break-lines
;;   '(push 'haskell-mode page-break-lines-modes))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

;;------------------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------------------
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
                                  (define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
                                  (define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
                                  (define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
                                   (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


(defun haskell-process-all-types ()
  "List all types in a grep-mode buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                 (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)))))



(provide 'init-haskell)
