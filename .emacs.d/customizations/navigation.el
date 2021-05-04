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
  (add-to-list 'recentf-exclude "\\.emacs\\.d/var/.*")
  (add-to-list 'recentf-exclude "/usr/local/Cellar/.*")
  (add-to-list 'recentf-exclude "elfeed/db/index")
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
         ("DEL" . evil-scroll-page-up))
  :config
  (setq dired-ls-F-marks-symlinks t
        delete-by-moving-to-trash t
        ))

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

;; https://fuco1.github.io/2017-07-15-Collapse-unique-nested-paths-in-dired-with-dired-collapse-mode.html
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

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
    ("q" nil)))

(use-package window-numbering
  :init (window-numbering-mode 1))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-project-search-path '("~/code/" "~/gh/" "~/code/antfin/"))
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

(use-package treemacs-projectile
  :defer t)
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode))

;; Customization
(defun my/switch-to-dependency-file ()
  (interactive)
  (let ((basename (pcase major-mode
                    ('go-mode "go.mod")
                    ('rust-mode "Cargo.toml")
                    ('clojure-mode "project.clj")
                    ('java-mode "pom.xml")
                    ('emacs-lisp-mode "init.el")
                    (mode nil))))

    (if basename
        (let ((metadata-dir (locate-dominating-file buffer-file-name basename)))
          (when metadata-dir
            (find-file (concat metadata-dir basename))))
      (message "%s isn't support for my/switch-to-metadata-file" major-mode))))

(use-package reveal-in-osx-finder
  :defer t)

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  (defun my/exec-shell-on-buffer (shell-command-text)
    (interactive "MShell command: ")
    (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))

  (defun my/imenu-dispatch ()
    (interactive)
    (if (bound-and-true-p lsp-mode)
        (my/toggle-treemacs-symbols)
      (counsel-imenu)))

  (transient-define-prefix my/lsp-command
    "LSP"
    [["Find"
      ("d" "Definition" lsp-find-definition)
      ("r" "References" lsp-find-references)
      ("i" "Implementation" lsp-find-implementation)
      ("c" "Call hierarchy" lsp-treemacs-call-hierarchy)]
     ["Edit"
      ("n" "reName" lsp-rename)
      ("m" "iMenu" my/imenu-dispatch)
      ("f" "auto Fix" lsp-execute-code-action)
      ("e" "Error list" flycheck-list-errors)
      ("t" "desc Thing" lsp-describe-thing-at-point)
      ]
     ["Rust"
      ("M" "Macroexpand" lsp-rust-analyzer-expand-macro)]])
  (transient-define-prefix my/file-command
    "Files"
    [["Find"
      ("f" "find-file" counsel-find-file)
      ("g" "git" counsel-git)
      ("p" "project" projectile-find-file)]
     ["Current File"
      ("s" "Save" save-buffer)
      ("y" "Copy Filename" my/copy-current-filename-to-clipboard)
      ("r" "Rename" my/rename-current-buffer-file)
      ("k" "Delete" my/delete-file-and-buffer)
      ("d" "Diff buffer" my/diff-buffer-with-file)
      ("t" "Last update" my/last-save-time)
      ("e" "Exec shell" my/exec-shell-on-buffer)]
     ["Edit"
      ("id" "insert date" my/insert-today)
      ("it" "insert time" my/insert-current-date-time)]])
  (transient-define-prefix my/search-command
    "Search"
    [["Search"
      ("s" "Search" swiper-isearch)
      ("a" "Agit" counsel-git-grep)
      ("r" "Rg" counsel-rg)
      ("v" "aVy-word" avy-goto-word-1)]
     ["Replace"
      ("f" "query-replace" query-replace)]])
  (transient-define-prefix my/projectile-command
    "Projectile"
    [["Find"
      ("f" "File" projectile-find-file)
      ("F" "File Other Window" projectile-find-file-other-window)
      ("s" "Ripgrep" projectile-ripgrep)
      ;; ("r" "Recentf" projectile-recentf)
      ("d" "Dired" projectile-dired)
      ("v" "discoVer" projectile-discover-projects-in-search-path)
      ("o" "multi occur" projectile-multi-occur)]
     ["Manage"
      ("p" "switch" projectile-switch-project)
      ("a" "add" projectile-add-known-project)
      ("i" "info" projectile-project-info)
      ("t" "test" projectile-test-project)
      ("c" "compile" projectile-compile-project)
      ("r" "run" projectile-run-project)]
     ["Treemacs"
      ("P" "Switch" treemacs-projectile)
      ("A" "Add" treemacs-add-project-to-workspace)]])
  (transient-define-prefix my/magit-command
    "Magit"
    [["Repository"
      ("s" "Status" magit-status)
      ("c" "Clone" magit-clone)
      ("L" "List Repositories" magit-list-repositories)
      ("d" "Dispatch Popup" magit-dispatch)]
     ["History"
      ("l" "File Popup" magit-log)
      ("b" "Blame" magit-blame-addition)
      ("j" "Blob Next" magit-blob-next)
      ("k" "Blob Previous" magit-blob-previous)]
     ["Files"
      ("p" "File Popup" magit-file-dispatch)
      ("f" "Find File" magit-find-file)
      ("F" "Find File in Other Window" magit-find-file-other-window)]])
  (transient-define-prefix my/progn-command
    "Progn"
    [["Edit"
      ("r" "Query Replace" query-replace)
      ("t" "Insert Today" my/insert-today)
      ("i" "Insert ISO8601" my/insert-current-date-time)
      ("d" "Datetime<->ts" my/timestamp->human-date)
      ("w" "Ispell Word" ispell-word)]
     ["System"
      ("F" "Finder" reveal-in-osx-finder)
      ("s" "Shell" my/open-terminal)
      ("f" "Fanyi" osx-dictionary-search-pointer)
      ("t" "Treemacs" treemacs)
      ("e" "Epa" my/epa-command)]
     ["Goto"
      ("m" "Mark Ring" counsel-mark-ring)
      ("n" "Dependency files" my/switch-to-dependency-file)
      ("SPC" "Avy" avy-goto-word-1)
      ("c" "lk-commit" my/git-link)
      ("h" "lk-homepage" git-link-homepage)]]
    [:hide (lambda () t)
     (":" eval-expression)
     ("'" eval-expression)])
  (transient-define-prefix my/window-command
    "Window"
    [["Split"
      ("-" "Below" split-window-below)
      ("|" "Right" split-window-right)
      ("\\" "Right" split-window-right)]
     ["Move"
      ("h" "Left" evil-window-left)
      ("l" "Right" evil-window-right)
      ("k" "Up" evil-window-up)
      ("j" "Down" evil-window-down)
      ("." "Next buffer" evil-next-buffer)
      ("," "Prev buffer" evil-prev-buffer)]
     ["Delete"
      ("<backspace>" "Del Win" delete-window)
      ("DEL" "Del Win" delete-window) ;; For terminals.
      ("x" "Kill buffer" kill-buffer)]]
    [:hide (lambda () t)
     ("e" eval-buffer)])

  :custom ((evil-leader/leader ",")
           (evil-leader/no-prefix-mode-rx '("magit.*" "mu4e.*" "dashboard-mode" "elfeed.*" "dired.*"))
           (evil-leader/in-all-states t))
  :config
  (defun my/insert-comma ()
    (interactive)
    (insert-char (char-from-name "COMMA")))

  (defun my/toggle-evil-emacs-mode ()
    (interactive)
    (if (eq evil-state 'emacs)
        (evil-exit-emacs-state)
      (evil-emacs-state)))

  (evil-leader/set-key
    "w" 'eww
    "e" 'tiny-expand
    "r" 'counsel-switch-buffer
    "u" 'mu4e
    "i" 'elfeed-dashboard
    "p" 'my/projectile-command

    "a" 'swiper-isearch
    "s" 'my/search-command
    "d" 'my/file-command
    "f" 'counsel-find-file
    "g" 'my/magit-command
    "h" 'my/major-mode-keymap
    "j" 'my/progn-command
    "k" 'kill-buffer
    "l" 'my/lsp-command

    "z" 'my/toggle-evil-emacs-mode
    "x" 'counsel-rg
    "c" 'compile
    "v" 'counsel-org-capture
    "b" 'counsel-bookmark
    "m" 'hydra-multiple-cursors/body
    "," 'my/insert-comma
    "." 'my/window-command

    "SPC" 'avy-goto-word-1
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4
    "8" 'cfw:open-calendar-buffer
    "9" 'calendar
    ))
