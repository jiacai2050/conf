;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet

(use-package paredit)

(use-package smartparens
     :config (require 'smartparens-config)
     :hook ((c++-mode c-mode python-mode
                      ruby-mode js2-mode tuareg-mode
                      go-mode rust-mode) . smartparens-mode))

(use-package flycheck
  ;; :pin melpa-stable
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc rust-cargo rust rust-clippy))
  (setq flycheck-python-flake8-executable "flake8")
  )

;; magit dependencies
(use-package magit-popup)
(use-package with-editor)
(use-package dash)
(use-package transient)
(use-package ghub)

(use-package magit
  :load-path "~/.emacs.d/vendor/magit/lisp"
  :after (with-editor transient dash magit-popup ghub)
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-blame-addition))
  ;; :config (setq magit-completing-read-function 'magit-ido-completing-read)
)

(use-package git-link
  :bind (("C-c g l" . git-link)
         ("C-c g c" . my/git-link-commit)
         ("C-c g h" . git-link-homepage))
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
  :bind (("C-c g t" . git-timemachine-toggle)))

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/codes/git"))

(use-package lsp-mode
  :load-path "~/.emacs.d/vendor/lsp-mode"
  ;; :load-path "~/code/misc/lsp-mode"
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

(use-package lsp-treemacs
  :bind (:map lsp-mode-map
              ("C-c C-u" . lsp-treemacs-symbols)))

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

(my/global-map-and-set-key "C-R" 'my/playground-exec)
(global-set-key (kbd "<C-return>") 'my/playground-exec)
