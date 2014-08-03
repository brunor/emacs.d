;; (setq auto-mode-alist
;;       (append '(("SConstruct\\'" . python-mode)
;; 		("SConscript\\'" . python-mode))
;;               auto-mode-alist))

(require-package 'elpy)
(elpy-enable)
;;(elpy-clean-modeline)
(elpy-use-ipython)
(defalias 'workon 'pyvenv-workon)
;; fixing a key binding bug in elpy
;;(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; fixing another key binding buf in iedit mode
;;(define-key global-map (kbd "C-c o") 'iedit-mode)
(setq elpy-rpc-backend "jedi")

(provide 'init-python-mode)
