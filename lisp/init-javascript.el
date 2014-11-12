;;; settings for javascript develpment

(require-package 'json-mode)
(require-package 'js2-mode)
(require-package 'ac-js2)
(require-package 'coffee-mode)
(require-package 'js-comint)
(require-package 'tern)
(require-package 'tern-auto-complete)
(require-package 'jsx-mode)
(require-package 'web-beautify)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


(define-derived-mode jsx2-mode js2-mode "jsx2" "JSX mode based on js2")

(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx2-mode))

(require 'flycheck)

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" (config-file "--config=" jshint-configuration-path) source)
  :error-patterns ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode jsx2-mode))

(defun find-jshintrc ()
  (expand-file-name ".jshintrc"
                    (locate-dominating-file
                     (or (buffer-file-name) default-directory) ".jshintrc")))


(defun setup-jsxhint ()
  (setq-local jshint-configuration-path (find-jshintrc))
  (flycheck-select-checker 'jsxhint-checker)
  (flycheck-mode))

(add-hook 'jsx2-mode-hook 'setup-jsxhint)
(add-hook 'jsx-mode-hook 'setup-jsxhint)
;; (require-package 'flycheck)
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (jsx-mode))

;; (add-hook 'jsx-mode-hook (lambda ()
;;                            (flycheck-select-checker 'jsxhint-checker)
;;                            (flycheck-mode)))

;; (add-hook 'jsx-mode-hook
;;           (lambda () (auto-complete-mode 1)))

(setq jsx-indent-level 2)

;; js2-mode
(after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(setq-default
 js2-basic-offset preferred-javascript-indent-level
 js2-bounce-indent-p nil)

(after-load 'js2-mode
  (js2-imenu-extras-setup))

(setq js2-highlight-level 3)

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; set up tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(add-hook 'jsx-mode-hook (lambda () (tern-mode t)))

;; Sometimes when you have just added .tern-project file or edit the file but Tern does not auto reload,
;; you need to manually kill Tern server. This little piece of code does the trick
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))


;; Javascript nests {} and () a lot, so I find this helpful

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))



;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "js")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (and (>= emacs-major-version 24) (featurep 'js2-mode))
  (require-package 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))


(provide 'init-javascript)
