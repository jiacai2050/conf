;; Customizations relating to editing a buffer.

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

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

(setq electric-indent-mode nil)
(browse-kill-ring-default-keybindings)
(add-hook 'after-init-hook 'global-company-mode)
(setq column-number-mode t)

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
(global-set-key (kbd "C-c r") 'my/rename-this-buffer-and-file)

(defvar my/iso-8601-format "%Y-%m-%dT%H:%M:%S%z")
(defun my/insert-current-date-time ()
  (interactive)
  (insert (format-time-string my/iso-8601-format (current-time))))

(defun my/insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))

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
          (kill-region (mark) (point))
          (insert (format-time-string my/iso-8601-format
                                      (seconds-to-time fixed-ts))))
      (deactivate-mark))))

(global-set-key (kbd "C-c d") 'my/timestamp->human-date)
(global-set-key "\C-c\C-d" 'my/insert-current-date-time)
(global-set-key "\C-c\C-t" 'my/insert-current-time)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(defun my/global-map-and-set-key (key command &optional prefix suffix)
   "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
 PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
   (my/map-key key)
   (global-set-key (kbd (concat prefix key suffix)) command))

 (defun my/map-key (key)
   "Map KEY from escape sequence \"\e[emacs-KEY\."
   (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

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


;; (desktop-save-mode 1)
;; (setq history-length 250)
(require 'minimal-session-saver)
(minimal-session-saver-install-aliases)
(setq minimal-session-saver-store-on-exit t)
(setq persistent-scratch-autosave-interval 5)
(ignore-errors
  (persistent-scratch-setup-default))
(global-set-key (kbd "C-c j") 'json-reformat-region)


(setq org-log-done 'time)
(setq org-startup-folded "showall")
(setq org-startup-indented t)
;; markdown export require emacs 25 https://stackoverflow.com/a/33033533/2163429
(require 'ox-md nil t)
;; terminal emacs can't display those lovely images :-(
;; (setq org-startup-with-inline-images t)
(defun my/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(require 'smartparens-config)
(add-hook 'enh-ruby-mode-hook 'smartparens-mode)
(add-hook 'js2-mode-hook 'smartparens-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
