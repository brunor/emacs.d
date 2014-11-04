(require-package 'ggtags)


(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))


(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (condition-case nil
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
          (buffer-substring (point-min) (1- (point-max)))
        nil))
    ((debug error) nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (condition-case nil
      (call-process "global" nil nil nil "-u")
    ((debug error) nil)))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(add-hook 'after-save-hook #'gtags-update-hook)

(provide 'init-gtags)
