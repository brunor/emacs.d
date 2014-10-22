(require-package 'xcscope)
(semantic-mode 1)
(require 'semantic/bovine/gcc)

(setq c-default-style "linux"
      c-basic-offset 4
      c-indent-level 4)

;;(semanticdb-enable-cscope-databases)

(provide 'init-cedet)
