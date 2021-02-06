;;; -*- lexical-binding: t; -*-

(use-package pyenv-mode
  :config
  (pyenv-mode)
  (add-hook 'python-mode-hook 'pyenv-mode)
  )

;; format, use lsp-format-buffer instead
;; (use-package py-autopep8
;;   :ensure-system-package ("autopep8" . "pip install autopep8")
;;   )
