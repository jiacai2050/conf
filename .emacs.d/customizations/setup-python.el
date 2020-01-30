(defun my/python-shell-send-buffer ()
  "Wrap python-shell-send-buffer, with send-main is true."
  (interactive)
  (python-shell-send-buffer t))

(use-package pyenv-mode
  :config
  (pyenv-mode)
  (add-hook 'python-mode-hook 'pyenv-mode)
  )

(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-c" . my/python-shell-send-buffer))
  )

;; format, use lsp-format-buffer instead
;; (use-package py-autopep8
;;   :ensure-system-package ("autopep8" . "pip install autopep8")
;;   )
