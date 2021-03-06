
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Emacs version is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

(require 'init-windows)
(require 'init-search)

(require 'init-flycheck)
(require 'init-company)
;;(require 'init-auto-complete)
(require 'init-sessions)
;;(require 'init-mmm)
(require 'init-editing-utils)
(require 'init-vc)

(require 'init-gtags)
(require 'init-markdown)
(require 'init-csv)

(require 'init-web)
(require 'init-javascript)
(require 'init-org)
(require 'init-nxml)
;;(require 'init-html)
(require 'init-css)
(require 'init-haml)
;;(require 'init-python-mode)
(require 'init-haskell)
(require 'init-sql)
(require 'init-yaml)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)

(require 'init-cedet)
(require 'init-ecb)

;; misc stuff prob too long to fit in this file
;; but doesnt currently have its own file
(require 'init-misc)

;;---------------------------------------------------------------------------
;; Extra packages which don't require any configuration
;;---------------------------------------------------------------------------
(require-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")
(require-package 'flymake-cursor) ; displays error in minibuffer
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(require-package 'regex-tool)
(require-package 'goto-last-change)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
