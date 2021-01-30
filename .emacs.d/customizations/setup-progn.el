;;; -*- lexical-binding: t; -*-

(use-package compile
  :custom (compilation-scroll-output t))

(use-package sql
  :ensure nil
  :hook ((sql-interactive-mode . my/sql-company))
  :config
  (defun my/sql-company ()
    (setq-local company-minimum-prefix-length 3)
    (setq-local company-backends
                '((company-dabbrev-code company-dabbrev company-tabnine))))
  )

;; third party extensions
(use-package sql-indent)

(use-package smartparens
  :init (defun my/sp-setup ()
          (smartparens-global-strict-mode 1))
  :hook (after-init . my/sp-setup)
  :custom (sp-base-key-bindings 'paredit)
  :config
  (progn
    (require 'smartparens-config)
    (define-key smartparens-mode-map (kbd "C-M-b") 'backward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-f") 'forward-sexp)
    (dolist (m '(org-mode org-agenda-mode)) ;; keybindings conflict
      (add-to-list 'sp-ignore-modes-list m)))
  :bind (:map smartparens-mode-map
              ("C-M-f" . forward-sexp)
              ("C-M-b" . backward-sexp)
              ("M-(" . sp-wrap-round)
              ("M-[" . sp-wrap-square)
              ("M-{" . sp-wrap-curly)))

(use-package flycheck
  ;; :pin melpa-stable
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc rust-cargo rust rust-clippy))
  (setq flycheck-python-flake8-executable "flake8")
  )

;; magit dependencies
(use-package with-editor
  :defer t)
(use-package dash
  :defer t)
(use-package transient
  :defer t
  :config
  (setq transient-history-file (expand-file-name "transient/history.el" my/ignore-directory)))

(use-package magit
  :load-path "~/.emacs.d/vendor/magit/lisp"
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)
         ("C-c j b" . magit-blame-addition)
         ("C-c j f" . magit-file-dispatch))
  :config
  (transient-bind-q-to-quit)
  :custom ((magit-diff-refine-hunk 'all)
           ;; https://emacs.stackexchange.com/a/3696/16450
           (find-file-visit-truename t)
           ;; https://emacs.stackexchange.com/a/52002/16450
           (transient-display-buffer-action '(display-buffer-below-selected))))

(use-package git-link
  :bind (("C-c j l" . git-link)
         ("C-c j c" . my/git-link-commit)
         ("C-c j h" . git-link-homepage))
  :config
  (progn
    (defun my/git-link-commit (remote start end)
      (interactive (let* ((remote (git-link--select-remote))
                          (region (when (or buffer-file-name (git-link--using-magit-blob-mode))
                                    (git-link--get-region))))
                     (list remote (car region) (cadr region))))
      (setq git-link-use-commit t)
      (git-link remote start end)
      (setq git-link-use-commit nil))
    (setq git-link-open-in-browser t)
    (add-to-list 'git-link-remote-alist
                 '("gitee\\.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
                 '("gitee\\.com" git-link-commit-github))
    (add-to-list 'git-link-remote-alist
                 '("alipay\\(-inc\\)?\\.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
                 '("alipay\\(-inc\\)?\\.com" git-link-commit-github))))

(use-package git-timemachine
  :bind (("C-c j t" . git-timemachine-toggle))
  :hook ((git-timemachine-mode . display-line-numbers-mode)
         (git-timemachine-mode . evil-normalize-keymaps))
  :config
  ;; https://github.com/emacs-evil/evil/issues/511
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  )

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/codes/git"))

(use-package lsp-mode
  :load-path "~/.emacs.d/vendor/lsp-mode"
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         ;; (sh-mode . lsp-deferred)
         ;; (js2-mode . lsp-deferred)
         )
  :ensure-system-package
  ((gopls . "go get golang.org/x/tools/gopls@latest")
   (pyls . "pip install 'python-language-server[all]'")
   ;; (typescript-language-server . "npm install -g typescript-language-server")
   ;; (bash-language-server . "npm install -g bash-language-server")
   )
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; https://github.com/emacs-lsp/lsp-mode/pull/1740
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (-let* (((&hash "value") contents)
            ;; (_  (message "hover value = [%s]" value))
            (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
            (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                           (-third-item groups)
                         (car groups)))
            (sig (--> sig_group
                      (--drop-while (s-equals? "```rust" it) it)
                      (--take-while (not (s-equals? "```" it)) it)
                      (s-join "" it))))
      ;; (message "sig = [%s]" sig)
      (lsp--render-element (concat "```rust\n" sig "\n```"))))
  :config
  ;; (add-hook 'before-save-hook 'lsp-format-buffer)
  (setq lsp-log-io nil
        lsp-session-file (expand-file-name "lsp-session-v1" my/ignore-directory)
        lsp-server-install-dir (expand-file-name "lsp-server" my/ignore-directory)
        lsp-eldoc-render-all nil
        lsp-completion-provider t
        ;; lsp-completion-enable nil
        lsp-signature-render-documentation nil
        lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-cargo-watch-enable nil
        lsp-gopls-hover-kind "NoDocumentation"
        lsp-gopls-use-placeholders t
        lsp-diagnostics-provider :none)
  (push "[/\\\\]vendor$" lsp-file-watch-ignored)
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references)
              ("C-c M-n" . lsp-rust-analyzer-expand-macro)
              ("C-c u" . lsp-execute-code-action)
              ("C-c d p" . lsp-describe-thing-at-point)))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :custom
  (dap-breakpoints-file (expand-file-name "dap-breakpoints" my/ignore-directory))
  (lsp-java-workspace-dir (expand-file-name "workspace" my/ignore-directory))
  (dap-java-test-runner (expand-file-name "eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar" my/ignore-directory))
  ;; 0.57.0 is the last version support jdk8. https://github.com/emacs-lsp/lsp-java/issues/249
  ;; "http://mirrors.ustc.edu.cn/eclipse/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"
  (lsp-java-jdt-download-url "http://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  :init
  (setq lsp-java--download-root "https://gitee.com/liujiacai/lsp-java/raw/master/install/"))

(use-package lsp-treemacs
  :load-path "~/.emacs.d/vendor/lsp-treemacs"
  :config
  (defun my/toggle-treemacs-symbols ()
    (interactive)
    (if-let (buf (get-buffer lsp-treemacs-symbols-buffer-name))
        (kill-buffer buf)
      (lsp-treemacs-symbols)))
  :bind (:map lsp-mode-map
              ("C-c C-u" . my/toggle-treemacs-symbols)))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (defun my/toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (if (hs-already-hidden-p)
          (hs-show-block)
        (hs-hide-block))))
  :bind (:map prog-mode-map
              ("C-c o" . my/toggle-fold))
  )

(use-package ggtags
  :hook ((c-mode c++-mode java-mode) . ggtags-mode))

(use-package eldoc
  :ensure nil
  :init
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; bridge to go-playground and rust-playground
(defun my/playground-exec ()
  (interactive)
  (cond ((rust-playground-get-snippet-basedir)
         (rust-playground-mode)
         (rust-playground-exec))
        ((string-match-p (file-truename go-playground-basedir) (file-truename (buffer-file-name)))
         (go-playground-mode)
         (go-playground-exec))))

;; for terminal
(my/global-map-and-set-key "C-R" 'my/playground-exec)
;; for GUI
(global-set-key (kbd "<C-return>") 'my/playground-exec)
