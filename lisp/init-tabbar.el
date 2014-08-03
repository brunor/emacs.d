;; show buffers as tabs
(require-package 'tabbar)
(tabbar-mode 1)

(global-set-key (kbd "C-S-p") 'tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-forward-group)
(global-set-key (kbd "C-<") 'tabbar-backward)
(global-set-key (kbd "C->") 'tabbar-forward) ;; tabbar.el, put all the buffers on the tabs.

;; (setq tabbar-background-color "#268bd2") ;; the color of the tabbar background
;; (custom-set-faces
;;  '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
;;  '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
;;  '(tabbar-button-highlight ((t (:inherit tabbar-default))))
;;  '(tabbar-highlight ((t (:underline t))))
;;  '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
;;  '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
;;  '(tabbar-unselected ((t (:inherit tabbar-default)))))

(provide 'init-tabbar)
