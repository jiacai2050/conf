;; Customizations relating to editing a buffer.

(setq column-number-mode t)
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
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

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq electric-indent-mode nil)
(setq kill-do-not-save-duplicates t)
;; https://stackoverflow.com/a/24639415/2163429
(setenv "LANG" "en_US.UTF-8")

;; 以下为第三方插件配置

;; yay rainbows!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(use-package kill-ring-search
  :config (global-set-key "\M-\C-y" 'kill-ring-search))
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package company
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay .3
        company-begin-commands '(self-insert-command)
        company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-dabbrev-downcase nil)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-i" . company-complete-selection)))

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

(defvar my/iso-8601-format "%Y-%m-%dT%H:%M:%S%z")
(defun my/insert-current-date-time ()
  (interactive)
  (insert (format-time-string my/iso-8601-format (current-time))))

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
          (insert (format-time-string my/iso-8601-format
                                      (seconds-to-time fixed-ts))))
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

(global-set-key (kbd "C-c r") 'my/rename-this-buffer-and-file)
(global-set-key (kbd "C-c i d") 'my/insert-current-date-time)
(global-set-key (kbd "C-c i t") 'my/insert-today)
(global-set-key (kbd "<f5>") 'my/zoom-in)
(global-set-key (kbd "<f6>") 'my/zoom-out)
(global-set-key (kbd "C-c h t") 'my/timestamp->human-date)
(global-set-key (kbd "C-c h u") 'my/url-decode-region)
(global-set-key (kbd "C-c h s") 'my/storage-size->human)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; https://emacs.stackexchange.com/questions/39129/multiple-cursors-and-return-key
(use-package multiple-cursors
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package expand-region)

(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

(defun my/global-map-and-set-key (key command &optional prefix suffix)
  "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
   (my/map-key key)
   (global-set-key (kbd (concat prefix key suffix)) command))

;; 需要配合 iTerm2 进行 key mapping
;; https://stackoverflow.com/a/40222318/2163429
(my/global-map-and-set-key "C-=" 'er/expand-region)
(my/global-map-and-set-key "C--" 'er/contract-region)
(my/global-map-and-set-key "C->" 'mc/mark-next-like-this)
(my/global-map-and-set-key "C-<" 'mc/mark-previous-like-this)
(my/global-map-and-set-key "C-c C->" 'mc/mark-all-like-this)
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c l") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c j") 'json-pretty-print)





;; (desktop-save-mode 1)
;; (setq history-length 250)
;; (require 'minimal-session-saver)
;; (minimal-session-saver-install-aliases)
;; (setq minimal-session-saver-store-on-exit t)
(use-package persistent-scratch
  :config
  (setq persistent-scratch-autosave-interval 5)
  (setq persistent-scratch-save-file
        (concat user-emacs-directory
                (if (display-graphic-p) ".persistent-scratch_gui" ".persistent-scratch_terminal")))
  (ignore-errors
    (persistent-scratch-setup-default)))

(use-package smartparens
  :config (require 'smartparens-config)
  :hook ((c++-mode c-mode python-mode
                   ruby-mode js2-mode tuareg-mode
                   go-mode rust-mode) . smartparens-mode))

(use-package yaml-mode
  :mode "\\.yml\\|ymal\\'")

(electric-indent-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
  :bind (
         ("M-i" . symbol-overlay-put)
         ("<f7>" . symbol-overlay-mode)
         ("<f8>" . symbol-overlay-remove-all))
  )

;;; editing.el ends here
