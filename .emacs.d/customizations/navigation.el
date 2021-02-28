;;; -*- lexical-binding: t; -*-

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-menu-items 40
        recentf-max-saved-items 150)
  (add-to-list 'recentf-exclude "\\.emacs\\.d/elpa/.*")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/ignore/.*")
  (add-to-list 'recentf-exclude "/usr/local/Cellar/.*")
  (add-to-list 'recentf-exclude "/Applications/.*")
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  (recentf-mode +1))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-alh"))
  :bind (:map dired-mode-map
              ("e" . dired-toggle-read-only)
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ("SPC" . evil-scroll-page-down)
              ("DEL" . evil-scroll-page-up)))

;; https://stackoverflow.com/a/950553/2163429
(global-visual-line-mode)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; move window by shift + up/down/left/right key
(windmove-default-keybindings)

(defun my/other-window-backward ()
  "Goto previous window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "\C-x i") 'my/other-window-backward)

;; Third party package

;; counsel ivy swiper
(use-package counsel
  :init
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)))
  :custom ((ivy-use-virtual-buffers t)
           (ivy-count-format "(%d/%d) ")
           (ivy-initial-inputs-alist nil)
           (ivy-height 15)
           (ivy-extra-directories '("./"))
           (counsel-switch-buffer-preview-virtual-buffers nil))
  :bind (("M-y" . counsel-yank-pop)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x f" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package ivy-avy
  :custom ((avy-all-windows nil)
           (avy-keys (number-sequence ?a ?z)))
  :bind (("C-x SPC" . avy-goto-char)
         ("C-c C-l" . avy-goto-line)
         ("C-C SPC" . avy-goto-word-1)))

(use-package ivy-hydra
  :config
  (defhydra hydra-prog-menu (:color pink :hint nil)
    "
^Edit^                ^Human^               ^System^
^^^^^^----------------------------------------------------
_r_: query-replace   _d_: datetime<->ts    _s_: shell
_t_: insert today    _v_: volume           _c_: lk-commit
_i_: insert iso8601  _j_: json             _h_: lk-home
_=_: +               ^ ^                   _g_: lk-git
_-_: -               ^ ^                   _f_: fanyi
^ ^                  ^ ^                   _e_: epa
"
    ("r" query-replace :exit t)
    ("t" my/insert-today :exit t)
    ("i" my/insert-current-date-time :exit t)
    ("=" er/expand-region)
    ("-" er/contract-region)

    ("d" my/timestamp->human-date)
    ("v" my/storage-size->human)
    ("j" my/format-json :exit t)

    ("s" shell :exit t)
    ("c" my/git-link-commit :exit t)
    ("h" git-link-homepage :exit t)
    ("g" git-link :exit t)
    ("f" go-translate :exit t)
    ("e" my/epa-command :exit t)

    ("q" nil))
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("|" mc/vertical-align)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil))
  )

(use-package window-numbering
  :init (window-numbering-mode 1))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-switch-project-action #'projectile-find-file-dwim
        projectile-completion-system 'ivy
        ;; projectile-enable-caching t
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                 projectile-root-bottom-up
                                                   projectile-root-local)
        projectile-ignored-project-function (lambda (project-root)
                                              (cl-dolist (deny '("\\.git" "\\.rustup" "\\.cargo" "go/pkg" "vendor" ".emacs.d/ignore" ".emacs.d/elpa"))
                                                (when (string-match-p deny project-root)
                                                  (cl-return t))))))

(use-package smex
  :config
  (smex-initialize))

(use-package rg
  :defer t)

(use-package treemacs
  :bind (("<f12>" . treemacs)
         ("M-0" . treemacs-select-window)
         :map treemacs-mode-map
         ("j" . treemacs-next-line)
         ("k" . treemacs-previous-line))
  :config
  (progn
    (evil-make-overriding-map treemacs-mode-map 'normal)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-projectile)
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode))

;; Customization

(use-package matcha
  :load-path "~/.emacs.d/vendor/matcha"
  :config
  (matcha-setup))

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  (defun my/exec-shell-on-buffer (shell-command-text)
    (interactive "MShell command: ")
    (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))

  (transient-define-prefix my/lsp-command
    "LSP"
    [["Find"
      ("d" "Definition" lsp-find-definition)
      ("r" "References" lsp-find-references)
      ("i" "Implementation" lsp-find-implementation)
      ("c" "Call hierarchy" lsp-treemacs-call-hierarchy)]
     ["Other"
      ("t" "Desc thing" lsp-describe-thing-at-point)
      ("s" "Desc session" lsp-describe-session)
      ("m" "Imenu" my/toggle-treemacs-symbols)
      ("f" "Quick fix" lsp-execute-code-action)
      ("l" "List error" flycheck-list-errors)]
     ["Rust"
      ("e" "Macroexpand" lsp-rust-analyzer-expand-macro)]])
  (transient-define-prefix my/file-command
    "Files"
    [["Find"
      ("f" "find-file" counsel-find-file)
      ("g" "git" counsel-git)
      ("p" "project" projectile-find-file)]
     ["Current File"
      ("s" "Save" save-buffer)
      ("y" "Copy Filename" matcha-copy-current-filename-to-clipboard)
      ("r" "Rename" matcha-rename-current-buffer-file)
      ("k" "Delete" my/delete-file-and-buffer)
      ("d" "Diff buffer" my/diff-buffer-with-file)
      ("t" "Last update" my/last-save-time)
      ("e" "Exec shell" my/exec-shell-on-buffer)]
     ["Edit"
      ("id" "insert date" my/insert-today)
      ("it" "insert time" my/insert-current-date-time)
      ]
     ["All Files"
      ("S" "Save All to SavedFile" matcha-save-files-to-saved-files-list)
      ("O" "Open All from SavedFile" matcha-open-files-from-saved-files-list)
      ("R" "Revert/Refresh All" matcha-revert-all-file-buffers)]])
  (transient-define-prefix my/search-command
    "Search"
    [["Search"
      ("s" "search" swiper-isearch)
      ("g" "git" counsel-git-grep)
      ("r" "rg" counsel-rg)
      ("a" "avy-word" avy-goto-word-1)]
     ["Replace"
      ("f" "query-replace" query-replace)]
     ])
  :custom ((evil-leader/leader ",")
           (evil-leader/no-prefix-mode-rx '(".*"))
           (evil-leader/in-all-states t))
  :config
  (require 'matcha-me)
  (defun my/insert-comma ()
    (interactive)
    (insert-char (char-from-name "COMMA")))

  (evil-leader/set-key
    "c" 'compile
    "s" 'my/search-command
    "a" 'swiper-isearch
    "f" 'my/file-command
    "b" 'counsel-bookmark
    "r" 'counsel-switch-buffer
    "l" 'my/lsp-command
    "," 'my/insert-comma
    "h" 'my/major-mode-keymap

    "k" 'kill-buffer
    "d" 'counsel-dired
    "j" 'hydra-prog-menu/body
    "m" 'hydra-multiple-cursors/body
    "u" 'mu4e
    "i" 'elfeed-dashboard
    "SPC" 'avy-goto-word-1
    "e" 'tiny-expand

    "." 'matcha-me-space
    "p" 'matcha-projectile
    "g" 'matcha-magit
    "v" 'matcha-vc-dir

    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4
    "8" 'cfw:open-calendar-buffer
    "9" 'calendar
    ))

(defun my/switch-to-metadata-file ()
  (interactive)
  (let ((basename (pcase major-mode
                    ('go-mode "go.mod")
                    ('rust-mode "Cargo.toml")
                    ('clojure-mode "project.clj")
                    ('java-mode "pom.xml")
                    (mode nil))))

    (if basename
        (let ((metadata-dir (locate-dominating-file buffer-file-name basename)))
          (when metadata-dir
            (find-file (concat metadata-dir basename))))
      (message "%s isn't support for my/switch-to-metadata-file" major-mode))))

;; matcha-helper https://github.com/jojojames/.emacs.d/blob/8414ea5253/config/jn-functions.el
(defun j-open-terminal ()
  "Open system terminal."
  (interactive)
  (cond
   (MAC-P
    (shell-command
     ;; open -a Terminal doesn't allow us to open a particular directory unless
     ;; We use --args AND -n, but -n opens an entirely new Terminal application
     ;; instance on every call, not just a new window. Using the
     ;; bundle here always opens the given directory in a new window.
     (concat "open -b com.apple.terminal " default-directory) nil nil))
   (WINDOWS-P
    ;; https://stackoverflow.com/questions/13505113/how-to-open-the-native-cmd-exe-window-in-emacs
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
   (t
    (message "Implement `j-open-terminal' for this OS!"))))

(use-package reveal-in-osx-finder)
(use-package emr)

(defun j-explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (cond
   ((and MAC-P
         (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and WINDOWS-P
         (fboundp #'explorer))
    (explorer))
   (LINUX-P
    (if (executable-find "gio")
        (progn
          (shell-command (format "gio open %s" default-directory))
          (message (format "Opened %s in file browser!" default-directory)))
      (message "On platform Linux but executable gio not found!")))
   (t
    (message "Implement `explorer-finder' for this OS!"))))

(defun j-resize-window ()
  "Resize window to fit contents."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer))))

(defun toggle-window-split ()
  "Toggles window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows-helper (x d)
  "Rotates windows."
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x)))
    (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))
