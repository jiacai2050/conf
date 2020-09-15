;;; javascript / html


(setq js-indent-level 2)
(setq css-indent-offset 2)

;; (use-package js-mode
;;   :ensure nil)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package js2-mode
  :init
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
