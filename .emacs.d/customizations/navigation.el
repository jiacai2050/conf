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
  (setq recentf-save-file (concat my/ignore-directory "recentf")
        recentf-max-menu-items 40
        recentf-max-saved-items 150)
  (add-to-list 'recentf-exclude "\\.emacs\\.d/elpa/.*")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/ignore/.*")
  (add-to-list 'recentf-exclude "gh/dotfiles/\\.emacs\\.d/")
  (add-to-list 'recentf-exclude "/usr/local/Cellar/.*")
  (add-to-list 'recentf-exclude "/Applications/.*")
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  (recentf-mode +1)
  )

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (concat my/ignore-directory "bookmarks")
        bookmark-save-flag t)
  )

;; https://stackoverflow.com/a/950553/2163429
(global-visual-line-mode)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq default-directory "~/")

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
  ;; (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                ;; (my/recentf-open . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)
                                )
        ivy-extra-directories '("./"))


  :bind (("M-y" . counsel-yank-pop)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . counsel-org-capture)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c s a" . counsel-ag)
         ("C-c s s" . counsel-rg)
         ("C-c s f" . counsel-git)
         ("C-c s g" . counsel-git-grep)
         ("C-c b" . counsel-bookmark)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-recentf)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done))
  )

(use-package ivy-avy
  :custom ((avy-all-windows nil)
           (avy-keys (number-sequence ?a ?z)))
  :bind (("C-x SPC" . avy-goto-char)
         ("C-c C-l" . avy-goto-line)
         ("C-C SPC" . avy-goto-word-1)))

(use-package ivy-hydra)

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-switch-project-action #'projectile-find-file-dwim
        projectile-completion-system 'ivy
        ;; projectile-enable-caching t
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                         my/ignore-directory)
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
  (setq smex-save-file (expand-file-name "smex-items" my/ignore-directory))
  (smex-initialize))


(use-package ag
  :ensure-system-package (ag . "brew install the_silver_searcher"))

;; prefer treemacs over this
;; (use-package sr-speedbar
;;   :config
;;   (setq speedbar-show-unknown-files t ;; show all files
;;         speedbar-use-images nil       ;; use text for buttons
;;         sr-speedbar-right-side nil    ;; put on left side
;;         sr-speedbar-width 30
;;         )
;;   :bind (("<f11>" . sr-speedbar-toggle)
;;          ("C-c s w" . sr-speedbar-select-window)
;;          ("C-c s r" . sr-speedbar-refresh-toggle)))

(use-package treemacs
  :bind (("C-c t" . treemacs)
         ("<f12>" . treemacs)
         ("M-0" . treemacs-select-window)
         :map treemacs-mode-map
         ("j" . treemacs-next-line)
         ("k" . treemacs-previous-line))
  :config
  (progn
    (setq treemacs-persist-file (concat my/ignore-directory "treemacs-persist"))
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

(global-set-key (kbd "C-c m") 'my/switch-to-metadata-file)
