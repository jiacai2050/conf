;;; javascript / html


(setq js-indent-level 2)

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
  :mode ("\\.js\\'" . js2-mode))

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . tagedit-mode))

;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
