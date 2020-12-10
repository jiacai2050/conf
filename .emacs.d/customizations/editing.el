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
  :bind ("C-c f t" . my/last-save-time)
  :config
  ;; Emacs can automatically create backup files. This tells Emacs to
  ;; put all backups in ~/.emacs.d/backups. More info:
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
  (setq backup-directory-alist `(("." . ,(concat my/ignore-directory
                                                 "backups")))
        version-control t
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
        auto-save-visited-interval 5)
  ;; don't forget the slash at the end of your string
  ;; https://emacs.stackexchange.com/a/17214/16450
  (setq auto-save-file-name-transforms
        `((".*" ,(concat my/ignore-directory "autosaves/") t))))

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
  (setq save-place-file (concat my/ignore-directory "places"))
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
              ("k" . dired-previous-line)))

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
        company-backends '((company-capf company-dabbrev-code)
                           (company-gtags company-etags company-keywords)
                           company-dabbrev
                           ))

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-i" . company-complete-selection)))

(use-package company-tabnine
  :custom ((company-tabnine-always-trigger nil))
  ;; :config
  ;; (add-to-list 'company-backends #'company-tabnine)
  )

(use-package multiple-cursors
  :bind (("C-c c l" . mc/edit-lines)
         ("C-c c e" . mc/edit-ends-of-lines)
         ("C-c c a" . mc/edit-beginnings-of-lines)
         ("C-c c g" . mc/mark-all-like-this)
         ("C-c c r" . set-rectangular-region-anchor)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this))
  ;; https://emacs.stackexchange.com/questions/39129/multiple-cursors-and-return-key
  ;; doesn't work in GUI
  :custom
  (mc/list-file (expand-file-name "mc-lists.el" user-emacs-directory))
  )

;; yay rainbows!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package expand-region)

(use-package persistent-scratch
  :config
  (setq persistent-scratch-autosave-interval 5)
  (setq persistent-scratch-save-file
        (concat my/ignore-directory
                (if (display-graphic-p)
                    "persistent-scratch_gui"
                  "persistent-scratch_terminal")))
  (ignore-errors
    (persistent-scratch-setup-default)))

(use-package yaml-mode
  :mode "\\.yml\\|ymal\\'")

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

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
  :ensure-system-package (vmd . "npm install -g vmd"))

(use-package carbon-now-sh)

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package go-translate
  :bind (("C-c f y" . go-translate))
  :config
  (setq go-translate-base-url "https://translate.google.cn"
        go-translate-extra-directions '(("zh-CN" . "en") ("en" . "zh-CN"))
        go-translate-local-language "auto"
        go-translate-target-language "auto"
        go-translate-buffer-follow-p t)
  )

(use-package evil
  :hook ((evil-mode . my/evil-keymap)
         (after-init . evil-mode))
  :custom ((evil-respect-visual-line-mode t))
  :init
  (defun my/evil-keymap ()
    (progn
      (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
      (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)
      (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
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
  (dolist (m '(dashboard-mode treemacs-mode dired-mode git-rebase-mode easy-hugo-mode))
    (add-to-list 'evil-emacs-state-modes m))
  (dolist (m '(wdired-mode))
    (add-to-list 'evil-normal-state-modes m))
  )

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "e"))

;; use 2 spaces for tabs
(defun my/die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; 以下为自定义函数
(defun my/rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

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
  (when (not mark-active)
    ;; require https://github.com/magnars/expand-region.el
    (er/mark-word))
  (letrec ((timestamp-str (buffer-substring (mark) (point)))
           (timestamp-int (string-to-number timestamp-str)))
    (if (> timestamp-int 0)
        (let ((fixed-ts (if (> timestamp-int (expt 10 11)) ;; 大于 10^11 为微秒，转为秒
                            (/ timestamp-int 1000)
                          timestamp-int)))
          ;; (kill-region (mark) (point))
          (end-of-line)
          (newline-and-indent)
          (insert (my/iso-8601-date-string (seconds-to-time fixed-ts))))
      (deactivate-mark))))

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
  (when (not mark-active)
    ;; require https://github.com/magnars/expand-region.el
    (er/mark-word))
  (letrec ((raw-size (string-to-number (buffer-substring (mark) (point)))))
    (while (> raw-size 1024)
      (setq raw-size (/ raw-size 1024.0)))
    (kill-region (mark) (point))
    (insert (format "%f" raw-size))
    (deactivate-mark)))

(defun my/reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

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

(defun my/filepath ()
  "copy buffer's full path to kill ring"
  (interactive)
  (let ((n (buffer-file-name)))
    (if n
        (progn
          (message "File path is %s" n)
          (kill-new n))
      (message "Not visit a file"))))

(defun my/diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (let ((diff-switches "-u")) ;; unified diff
    (diff-buffer-with-file (current-buffer))
    (other-window 1)))

(global-set-key (kbd "C-c k") 'my/delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'my/rename-this-buffer-and-file)
(global-set-key (kbd "C-c i d") 'my/insert-current-date-time)
(global-set-key (kbd "C-c i t") 'my/insert-today)
(global-set-key (kbd "C-c f p") 'my/filepath)
(global-set-key (kbd "C-c d f") 'my/diff-buffer-with-file)
(global-set-key (kbd "<f5>") 'my/zoom-in)
(global-set-key (kbd "<f6>") 'my/zoom-out)
(global-set-key (kbd "<f12>") 'view-mode)
(global-set-key (kbd "C-c h t") 'my/timestamp->human-date)
(global-set-key (kbd "C-c h u") 'my/url-decode-region)
(global-set-key (kbd "C-c h s") 'my/storage-size->human)
(global-set-key (kbd "C-c j") 'json-pretty-print)

;; 需要配合 iTerm2 进行 key mapping
;; https://stackoverflow.com/a/40222318/2163429
(my/global-map-and-set-key "C-=" 'er/expand-region)
(my/global-map-and-set-key "C--" 'er/contract-region)
(my/global-map-and-set-key "C->" 'mc/mark-next-like-this)
(my/global-map-and-set-key "C-<" 'mc/mark-previous-like-this)

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
