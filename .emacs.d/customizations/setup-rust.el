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
                compile-command "cargo check --color never --tests")))

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (defun my/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  :bind (:map rust-mode-map
              (("C-c C-t" . my/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)
           (cargo-process--command-flags "--  --nocapture")))

(provide 'setup-rust)
;;; setup-rust.el ends here
