(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  :init
  (elpy-enable)
  :config
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "M-,") 'xref-pop-marker-stack))

(use-package pyenv-mode
  :config
  (pyenv-mode)
  )

