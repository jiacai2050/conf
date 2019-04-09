
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  )

(use-package flycheck-rust
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :if (executable-find "racer")
  :hook ((rust-mode . racer-mode))
  :init
  (setq company-tooltip-align-annotations t))

(use-package company-racer
  :after racer
  :config (add-to-list 'company-backends 'company-racer))

(use-package cargo
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode)))

(provide 'setup-rust)
;;; setup-rust.el ends here

