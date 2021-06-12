(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/use-package")
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

(use-package no-littering
  :load-path "~/.emacs.d/vendor/no-littering"
  :config
  ;; don't forget the slash at the end of your string
  ;; https://emacs.stackexchange.com/a/17214/16450
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-var-file-name "custom.el")
        persistent-scratch-save-file (no-littering-expand-var-file-name
                                      (if (display-graphic-p)
                                          "scratch-gui.el"
                                        "scratch-terminal.el"))
        mc/list-file (no-littering-expand-etc-file-name "mc-lists.el"))

  (defun my/generate-autoloads (pkg-name &rest dirs)
    (setq generated-autoload-file (no-littering-expand-var-file-name (format "autoloads/%s-autoloads.el" pkg-name)))
    (let* ((autoload-timestamps nil)
           (backup-inhibited t)
           (version-control 'never))
      (unless (file-exists-p generated-autoload-file)
        (package-autoload-ensure-default-file generated-autoload-file)
        (apply 'update-directory-autoloads dirs))
      (load-file generated-autoload-file))))

(use-package company
  :load-path "~/.emacs.d/vendor/company-mode"
  :commands (global-company-mode)
  :init
  (global-company-mode t)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay .3
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-dabbrev-downcase nil
        ;; company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-gtags company-etags company-keywords)
                           (company-files)))

  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-i" . company-complete-selection)))

;; magit deps
(use-package with-editor
  :load-path "~/.emacs.d/vendor/with-editor"
  :commands with-editor-export-editor
  ;; sets up the with-editor package so things that invoke $EDITOR will use the current emacs if I’m already inside of emacs
  :hook ((shell-mode eshell-mode) . with-editor-export-editor))

(use-package dash
  :load-path "~/.emacs.d/vendor/dash.el"
  :defer t)

(use-package transient
  :load-path "~/.emacs.d/vendor/transient/lisp"
  :custom (;; https://emacs.stackexchange.com/a/52002/16450
           (transient-display-buffer-action '(display-buffer-below-selected)))
  :commands (transient-define-prefix)
  :config
  (transient-bind-q-to-quit))

;; lsp-treemacs deps
(use-package dash-functional
  :load-path "~/.emacs.d/vendor/dash.el"
  :defer t)

(use-package ht
  :load-path "~/.emacs.d/vendor/ht.el"
  :defer t)

(use-package s
  :load-path "~/.emacs.d/vendor/s.el"
  :defer t)

(use-package f
  :load-path "~/.emacs.d/vendor/f.el"
  :defer t)

;; lsp-mode deps
(use-package spinner
  :load-path "~/.emacs.d/vendor/spinner.el"
  :defer t)

(use-package lv
  :load-path "~/.emacs.d/vendor/hydra"
  :defer t)

;; easy-hugo deps
(use-package request
  :load-path "~/.emacs.d/vendor/emacs-request"
  :defer t)

;; unicode-escape deps
(use-package names
  :load-path "~/.emacs.d/vendor/names"
  :defer t)

;; company-tabnine deps, other: company, dash, s, cl-lib
(use-package unicode-escape
  :load-path "~/.emacs.d/vendor/unicode-escape.el"
  :defer t)

;; evil deps
(use-package goto-chg
  :load-path "~/.emacs.d/vendor/goto-chg"
  :defer t)

(use-package lsp-mode
  :load-path ("~/.emacs.d/vendor/lsp-mode" "~/.emacs.d/vendor/lsp-mode/clients")
  :init
  (my/generate-autoloads "lsp-mode"
                         "~/.emacs.d/vendor/lsp-mode"
                         "~/.emacs.d/vendor/lsp-mode/clients")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-js ()
    "Enable LSP for JavaScript, but not for JSON"
    (when (eq 'js-mode major-mode)
      (lsp-deferred)))
  (defun my/lsp-rust ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t)
    (setq-local lsp-completion-enable nil
                lsp-modeline-code-actions-enable nil)
    (lsp-deferred))
  ;; https://github.com/emacs-lsp/lsp-mode/pull/1740
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (-let* (((&hash "value") contents)
            (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
            (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                           (-third-item groups)
                         (car groups)))
            (sig (--> sig_group
                   (--drop-while (s-equals? "```rust" it) it)
                   (--take-while (not (s-equals? "```" it)) it)
                   (s-join "" it))))
      (lsp--render-element (concat "```rust\n" sig "\n```"))))
  :hook ((go-mode . lsp-deferred)
         (rust-mode . my/lsp-rust)
         (js-mode .  my/lsp-js)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp-rust-analyzer-expand-macro)
  :custom ((lsp-log-io nil)
           (lsp-eldoc-render-all nil)
           (lsp-completion-provider t)
           (lsp-signature-render-documentation nil)
           (lsp-rust-server 'rust-analyzer)
           (lsp-rust-analyzer-cargo-watch-enable nil)
           (lsp-go-hover-kind "NoDocumentation")
           (lsp-go-use-placeholders t)
           (lsp-diagnostics-provider :none)
           (lsp-modeline-diagnostics-enable nil)
           (lsp-headerline-breadcrumb-enable nil)
           (lsp-eslint-server-command `("node"
                                        ,(expand-file-name  "eslint/unzipped/extension/server/out/eslintServer.js" lsp-server-install-dir)
                                        "--stdio")))
  :config
  (push "[/\\\\]vendor$" lsp-file-watch-ignored-directories)
  :bind (:map lsp-mode-map
         ("M-." . lsp-find-definition)
         ("M-n" . lsp-find-references)))

;; (setq my/after-deps-loaded (current-time))
;; (message "Load deps.el cost %f seconds"
;; 		 (float-time
;; 		  (time-subtract my/after-deps-loaded my/after-early-init-loaded)))
