;;; -*- lexical-binding: t; -*-

(use-package rust-playground
  :after rust-mode
  :config
  (setq rust-playground-basedir (expand-file-name "~/code/rust/playground")))

(use-package rust-mode
  :hook ((rust-mode . my/rust-lsp))
  :config
  (setq rust-format-on-save t)
  (defun my/rust-lsp ()
    (setq-local lsp-completion-enable nil
                compile-command "cargo check")
    ))

;; (use-package flycheck-rust
;;   :after rust-mode
;;   :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; use lsp-mode instead
;; (use-package racer
;;   :after rust-mode
;;   :if (executable-find "racer")
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . eldoc-mode))
;;   :bind (:map rust-mode-map
;;               ("TAB" . company-indent-or-complete-common)))

(use-package cargo
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode))
  :bind (:map cargo-minor-mode-map
              (("C-c C-t" . cargo-process-current-test)))
  :config
  (setq cargo-process--command-flags "--  --nocapture"))

(provide 'setup-rust)
;;; setup-rust.el ends here
