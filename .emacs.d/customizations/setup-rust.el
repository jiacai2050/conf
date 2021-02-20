;;; -*- lexical-binding: t; -*-

(use-package rust-playground
  :after rust-mode
  :config
  (setq rust-playground-basedir (expand-file-name "~/code/rust/playground")))

(use-package rust-mode
  :hook (rust-mode . my/rust-compile)
  :config
  (defun my/rust-compile ()
    (setq-local company-backends '(company-tabnine company-dabbrev-code)
                compile-command "cargo check --tests")))

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :bind (:map rust-mode-map
              (("C-c C-t" . cargo-process-current-test)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--command-flags "--  --nocapture")))

(provide 'setup-rust)
;;; setup-rust.el ends here
