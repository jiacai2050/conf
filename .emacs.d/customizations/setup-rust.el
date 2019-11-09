
(use-package rust-playground
  :after rust-mode
  :config
  (setq rust-playground-basedir (expand-file-name "~/code/rust/playground"))
  ;; :hook (rust-mode . rust-playground-mode)
  ;; :bind (:map rust-playground-mode
  ;;             ("C-c C-c" . rust-playground-exec)
  ;;             ("C-c b" . rust-playground-switch-between-cargo-and-main)
  ;;             ("C-c k" . rust-playground-rm))
  (defun my/rust-playground-hook ()
    (my/global-map-and-set-key "C-R" 'rust-playground-exec))
  
  (add-hook 'rust-playground-mode-hook #'my/rust-playground-hook)
  )

(use-package rust-mode
  :after rust-playground
  :config
  (setq rust-format-on-save t)
  )

(use-package flycheck-rust
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :if (executable-find "racer")
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode))
  :bind (:map rust-mode-map
              ("TAB" . company-indent-or-complete-common)))

(use-package cargo
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (setq cargo-process--command-flags "--  --nocapture"))

(provide 'setup-rust)
;;; setup-rust.el ends here

