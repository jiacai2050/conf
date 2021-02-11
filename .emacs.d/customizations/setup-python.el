;;; -*- lexical-binding: t; -*-

(use-package pyenv-mode
  :init
  (defun my/set-python-exe ()
    (let ((my/python-exe (expand-file-name "~/.pyenv/shims/python")))
      (setq flycheck-python-pylint-executable my/python-exe
            flycheck-python-pycompile-executable my/python-exe
            flycheck-python-flake8-executable my/python-exe)))
  :hook ((python-mode . pyenv-mode)
         (python-mode . my/set-python-exe))
  )
