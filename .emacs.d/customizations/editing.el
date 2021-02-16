;;; -*- lexical-binding: t; -*-

;; Customizations relating to editing a buffer.

(setq column-number-mode t)
(electric-indent-mode)
(setq kill-do-not-save-duplicates t)
;; https://stackoverflow.com/a/24639415/2163429
(setenv "LANG" "en_US.UTF-8")
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Highlights matching parenthesis
(show-paren-mode 1)
;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(global-subword-mode)
;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; https://emacs.stackexchange.com/a/7889/16450
;; (defvar buffer-last-save-time nil "The last change time")
;; (make-variable-buffer-local 'buffer-last-save-time)
(defun my/last-save-time ()
  (interactive)
  (message "%s"
           (format-time-string "Last update %F %T"
                               (visited-file-modtime))
           ;; (buffer-name)
           ))

(use-package files
  :ensure nil
  :config
  (setq version-control t
        kept-new-versions 5
        kept-old-versions 3
        backup-by-copying-when-linked t
        backup-by-copying t
        vc-make-backup-files t
        delete-old-versions t)

  ;; auto save in original file
  ;; (auto-save-visited-mode +1)
  (setq auto-save-default t
        auto-save-timeout 10
        auto-save-interval 200
        auto-save-visited-interval 5))

(use-package executable
  :ensure nil
  :config
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  (setq executable-prefix-env t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1)
  (setq-default save-place t)
  )

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package view
  :ensure nil
  :bind (:map view-mode-map
              (("g" . goto-line)
               ("j" . next-line)
               ("k" . previous-line)
               ("n" . next-logical-line)
               ("p" . previous-logical-line))))

(use-package conf-mode
  :ensure nil
  :mode (("\\.gitconfig\\'" . conf-mode))
  :config
  (define-key conf-mode-map "\C-c " nil))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . dired-toggle-read-only)
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ("SPC" . evil-scroll-page-down)
              ("DEL" . evil-scroll-page-up)))

(use-package auth-source
  :ensure nil
  :custom ((auth-sources '("~/.emacs.d/authinfo.gpg"))))

(use-package epa
  :ensure nil
  :custom ((password-cache-expiry (* 60 15)))
  :config
    ;; (setq epa-file-encrypt-to "jiacai2050@gmail.com")

  (defun my/sign-file (&optional initial-input initial-directory)
    (interactive)
    (when-let (f (counsel-find-file (or initial-input (buffer-name))
                                    initial-directory))
      (let ((epa-armor t))
        (epa-sign-file f nil 'detached))))

  (transient-define-prefix my/epa-command ()
    [["Keys"
      ("l" "list public" epa-list-keys)
      ("m" "list secret" epa-list-secret-keys)
      ("u" "unmark" epa-unmark-key)
      ("r" "remove" epa-delete-keys)
      ("i" "import" epa-import-keys)
      ("o" "export" epa-export-keys)]
     ["File"
      ("d" "decrypt" epa-decrypt-file)
      ("v" "verify" epa-verify-file)
      ("s" "sign" my/sign-file)]]))

;; (use-package flyspell
;;   :hook ((text-mode . flyspell-mode)
;;          (prog-mode . flyspell-prog-mode))
;;   )

;; 以下为第三方插件配置

(use-package company
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 3
        company-idle-delay .3
        company-begin-commands '(self-insert-command)
        company-dabbrev-downcase nil
        ;; company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-backends '((company-tabnine company-capf company-dabbrev-code)
                           (company-gtags company-etags company-keywords)
                           company-dabbrev
                           ))

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-i" . company-complete-selection)))

(use-package company-tabnine
  :custom ((company-tabnine-always-trigger nil))
  )

(use-package multiple-cursors
  ;; https://emacs.stackexchange.com/questions/39129/multiple-cursors-and-return-key
  ;; doesn't work in GUI
  :bind (("C-." . mc/mark-next-like-this))
  :custom
  (mc/list-file (expand-file-name "mc-lists.el" user-emacs-directory))
  )

;; yay rainbows!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package expand-region
  :config
  ;; 需要配合 iTerm2 进行 key mapping
  ;; https://stackoverflow.com/a/40222318/2163429
  (my/global-map-and-set-key "C-=" 'er/expand-region)
  (my/global-map-and-set-key "C--" 'er/contract-region))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (:map undo-tree-visualizer-mode-map
              (("j" . undo-tree-visualize-redo)
               ("k" . undo-tree-visualize-undo)
               ("h" . undo-tree-visualize-switch-branch-left)
               ("l" . undo-tree-visualize-switch-branch-right))))

(use-package persistent-scratch
  :config
  (setq persistent-scratch-autosave-interval 5)
  (ignore-errors
    (persistent-scratch-setup-default)))

(use-package yaml-mode
  :mode "\\.yml\\|ymal\\'")

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package iedit
  :config
  (my/global-map-and-set-key "C-;" 'iedit-mode))

(use-package symbol-overlay
  :config (setq symbol-overlay-scope t)
  :bind (("M-i" . symbol-overlay-put)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vmd-mode
  :defer t)

(use-package carbon-now-sh
  :defer t)

(use-package go-translate
  :config
  (setq go-translate-base-url "https://translate.google.cn"
        go-translate-extra-directions '(("en" . "zh-CN"))
        go-translate-target-language "zh-CN"
        go-translate-local-language "en"
        go-translate-buffer-follow-p t
        go-translate-token-current (cons 430675 2721866130))
  )

(use-package evil-numbers
  :defer t)

(use-package evil
  :hook ((evil-mode . my/evil-keymap)
         (after-init . evil-mode))
  :custom ((evil-respect-visual-line-mode t)
           (evil-move-beyond-eol t))
  :commands (evil-make-overriding-map)
  :init
  (defun my/evil-keymap ()
    (progn
      (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
      (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)
      (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
      (define-key evil-normal-state-map (kbd "C-y") 'yank)
      (define-key evil-normal-state-map (kbd "C-f") 'forward-char)
      (define-key evil-normal-state-map (kbd "C-d") 'delete-char)
      (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
      (define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)
      (define-key evil-normal-state-map (kbd "RET") 'xref-goto-xref)
      (define-key evil-normal-state-map (kbd "M-;") 'comment-dwim)
      (define-key evil-normal-state-map (kbd "q") 'quit-window)
      (define-key evil-normal-state-map (kbd "C-M-b") 'backward-sexp)
      (define-key evil-normal-state-map (kbd "C-M-f") 'forward-sexp))

    (define-key evil-motion-state-map (kbd "SPC") 'evil-scroll-page-down)
    (define-key evil-motion-state-map (kbd "C-e") 'end-of-visual-line)
    (define-key evil-motion-state-map (kbd "C-a") 'beginning-of-visual-line)

    (progn
      (define-key evil-insert-state-map (kbd "C-y") 'yank)
      (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
      (define-key evil-insert-state-map (kbd "C-e") 'end-of-visual-line)
      (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-visual-line)
      (define-key evil-insert-state-map (kbd "C-k") 'kill-visual-line)
      (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
      (define-key evil-insert-state-map (kbd "C-v") 'scroll-up-command)
      (define-key evil-insert-state-map (kbd "C-n") 'next-line)
      (define-key evil-insert-state-map (kbd "C-p") 'previous-line)))

  :config
  (dolist (m '(dashboard-mode git-rebase-mode easy-hugo-mode
                              epa-key-list-mode epa-key-mode epa-info-mode))
    (add-to-list 'evil-emacs-state-modes m))
  (dolist (m '(wdired-mode))
    (add-to-list 'evil-normal-state-modes m))

  (require 'dired)
  (evil-make-overriding-map dired-mode-map 'normal)
  )

(use-package keyfreq
  :init (progn
          (keyfreq-mode 1)
          (keyfreq-autosave-mode 1))
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line))
  )

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "e"))

(use-package tiny
  :bind (("C-c ;" . tiny-expand)))

(use-package separedit
  :bind (:map prog-mode-map
			  (("C-c '" . separedit)))
  :config
  (add-hook 'separedit-buffer-creation-hook #'auto-fill-mode))

;; use 2 spaces for tabs
(defun my/die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; 以下为自定义函数
(defun my/iso-8601-date-string (&optional datetime)
  (concat
   (format-time-string "%Y-%m-%dT%T" datetime)
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z" datetime))))

(defun my/insert-current-date-time ()
  (interactive)
  (insert (my/iso-8601-date-string)))

(defun my/insert-today ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun my/timestamp->human-date ()
  (interactive)
  (unless (region-active-p)
    (set-mark (line-beginning-position))
    (goto-char (line-end-position)))
  (letrec ((date-string (buffer-substring (mark) (point)))
           (body (if (iso8601-valid-p date-string)
                     ;; date -> ts
                     (format-time-string "%s" (parse-iso8601-time-string date-string))
                   ;; ts -> date
                   (let ((timestamp-int (string-to-number date-string)))
                     (thread-last
                         (if (> timestamp-int (expt 10 11)) ;; 大于 10^11 为微秒，转为秒
                             (/ timestamp-int 1000)
                           timestamp-int)
                       (seconds-to-time)
                       (my/iso-8601-date-string))))))
    (unless (string-empty-p body)
      (end-of-line)
      (newline-and-indent)
      (insert body))
    (deactivate-mark)))

(defun my/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))
(defun my/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

(defun my/update-path (new-path)
  (interactive "sEnter a new path: ")
  (if (file-directory-p new-path)
      (progn
        (setenv "PATH" (concat (getenv "PATH") ":" new-path))
        (setq exec-path (append exec-path '((concat ":" new-path))))
        (message "%s added to PATH & exec-path" new-path))
    (message "%s not exists!")))

(defun my/url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun my/storage-size->human ()
  "Divide by 1024 for human"
  (interactive)
  (unless mark-active
    ;; require https://github.com/magnars/expand-region.el
    (er/mark-word))
  (letrec ((raw-size (string-to-number (buffer-substring (mark) (point)))))
    (while (> raw-size 1024)
      (setq raw-size (/ raw-size 1024.0)))
    (kill-region (mark) (point))
    (insert (format "%f" raw-size))
    (deactivate-mark)))

(defun my/format-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun my/format-json ()
  (interactive)
  (save-excursion
    (if mark-active
        (json-pretty-print (mark) (point))
      (json-pretty-print-buffer))))

(defun my/delete-file-and-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "bDelete file")
  (let* ((buffer (get-buffer buffername))
         (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)
      (kill-buffer))))

(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my/diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (let ((diff-switches "-u")) ;; unified diff
    (diff-buffer-with-file (current-buffer))
    (other-window 1)))

(defun my/derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from."
  (interactive (list major-mode))
  (defun iter (mode)
    (and mode
         (cons mode
               (iter (get mode 'derived-mode-parent)))))
  (message "%s" (iter mode)))

(global-set-key (kbd "C-c i d") 'my/insert-current-date-time)
(global-set-key (kbd "C-c i t") 'my/insert-today)
(global-set-key (kbd "<f5>") 'my/zoom-in)
(global-set-key (kbd "<f6>") 'my/zoom-out)

;; (use-package smart-input-source
;;   :config
;;   (setq smart-input-source-english-input-source "com.apple.keylayout.US"
;;         smart-input-source-other-input-source "com.apple.inputmethod.SCIM.ITABC")

;;   (add-hook 'text-mode-hook #'smart-input-source-mode)
;;   (add-hook 'prog-mode-hook #'smart-input-source-mode))

;; https://github.com/doublep/logview
;; (use-package logview
;;   :hook (logview-mode . read-only-mode)
;;     ;; (add-hook #'logview-mode-hook #'read-only-mode)
;;   )

;; https://github.com/m00natic/vlfi
;; (use-package vlf
;;   :config (require 'vlf-setup))

;;; editing.el ends here
