;;; -*- lexical-binding: t; -*-

;;; javascript / html

(use-package js
  :ensure nil
  :custom ((js-indent-level 2)))

(use-package json-mode
  :init (defun my/json-before-save()
          (add-hook 'before-save-hook 'json-pretty-print-buffer nil t))
  :mode (("\\.jshintrc" . json-mode))
  :hook (json-mode . my/json-before-save)
  )

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . tagedit-mode))

;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
