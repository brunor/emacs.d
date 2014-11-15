;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; recentf
;;----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))


;;----------------------------------------------------------------------------
;; textile-mode
;;----------------------------------------------------------------------------
(require-package 'textile-mode)

(autoload 'textile-mode "textile-mode" "Mode for editing Textile documents" t)
(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))


;;----------------------------------------------------------------------------
;;
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")



(provide 'init-misc)
