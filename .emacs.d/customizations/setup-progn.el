;;; -*- lexical-binding: t; -*-

(use-package compile
  :ensure nil
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

(use-package vc
  :ensure nil
  :config
  (define-key ctl-x-map "j" 'vc-prefix-map))

(use-package eldoc
  :ensure nil
  :init
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

(use-package hideshow
  :ensure nil
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

(use-package newcomment
  :ensure nil
  :config
  (setq-default comment-start "# "))

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
  :custom ((flycheck-checker-error-threshold 20))
  :config
  (global-flycheck-mode)
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-jshint)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp rust-cargo rust rust-clippy))
  )

(use-package magit
  :load-path "~/.emacs.d/vendor/magit/lisp"
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :custom ((magit-diff-refine-hunk 'all)
           ;; https://emacs.stackexchange.com/a/3696/16450
           (find-file-visit-truename t)))

(use-package forge
  ;; 1. first setup USERNAME
  ;; git config --global github.user USERNAME
  ;; 2. token
  ;; https://magit.vc/manual/ghub/Storing-a-Token.html#Storing-a-Token
  :after magit)

(use-package git-link
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
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook ((git-timemachine-mode . display-line-numbers-mode)
         (git-timemachine-mode . evil-normalize-keymaps))
  :config
  ;; https://github.com/emacs-evil/evil/issues/511
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  )

(use-package lsp-mode
  :load-path ("~/.emacs.d/vendor/lsp-mode" "~/.emacs.d/vendor/lsp-mode/clients")
  :init
  (defun my/lsp-before-save ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  (defun my/lsp-js ()
    "Enable LSP for JavaScript, but not for JSON"
    (when (eq 'js-mode major-mode)
      (lsp-deferred)))
  (defun my/lsp-rust ()
    (setq-local lsp-completion-enable nil
                lsp-modeline-code-actions-enable nil)
    (lsp-deferred))
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
  :hook ((go-mode . lsp-deferred)
         (rust-mode . my/lsp-rust)
         (python-mode . lsp-deferred)
         (js-mode .  my/lsp-js)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . my/lsp-before-save))
  :commands (lsp lsp-deferred)
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
           (lsp-eslint-server-command `("node"
                                        ,(expand-file-name  "eslint/unzipped/extension/server/out/eslintServer.js" lsp-server-install-dir)
                                        "--stdio")))
  :config
  (require 'lsp-modeline)
  (push "[/\\\\]vendor$" lsp-file-watch-ignored-directories)
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references)))

(use-package lsp-treemacs
  :load-path "~/.emacs.d/vendor/lsp-treemacs"
  :commands (lsp-treemacs-symbols lsp-treemacs-references
                                  lsp-treemacs-implementations lsp-treemacs-call-hierarchy)
  :init
  (defun my/toggle-treemacs-symbols ()
    (interactive)
    (if-let (buf (get-buffer lsp-treemacs-symbols-buffer-name))
        (kill-buffer buf)
      (lsp-treemacs-symbols)))
  :bind (:map lsp-mode-map
              ("C-c C-u" . my/toggle-treemacs-symbols)))

(comment
 (use-package lsp-java
   :hook (java-mode . lsp-deferred)
   :custom
   ;; 0.57.0 is the last version support jdk8. https://github.com/emacs-lsp/lsp-java/issues/249
   ;; "http://mirrors.ustc.edu.cn/eclipse/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"
   (lsp-java-jdt-download-url "http://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")
   :init
   (setq lsp-java--download-root "https://gitee.com/liujiacai/lsp-java/raw/master/install/")))

(use-package ggtags
  :hook ((c-mode c++-mode java-mode) . ggtags-mode))

(use-package graphviz-dot-mode
  :hook (graphviz-dot-mode . my/graphviz-company)
  :config
  (defun my/graphviz-company ()
    (add-to-list 'company-backends 'company-graphviz-dot-backend))
  (setq graphviz-dot-indent-width 4))

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
