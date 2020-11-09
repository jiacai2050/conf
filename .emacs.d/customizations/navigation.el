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

(global-set-key (kbd "\C-x p") 'my/other-window-backward)

;; Third party package

;; counsel ivy swiper
(use-package counsel
  :config
  (ivy-mode +1)
  ;; (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                ;; (my/recentf-open . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)
                                )
        ivy-extra-directories '("./"))

  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "C-c f f") 'counsel-git-grep)
  (global-set-key (kbd "C-c f s") 'counsel-git)
  (global-set-key (kbd "C-c b") 'counsel-bookmark)

  (defun my/recentf-open ()
    (interactive)
    (let ((file (ivy-read "Find recent file: " (mapcar 'abbreviate-file-name recentf-list))))
      (if (find-file file)
          (message "Opening file %s" (abbreviate-file-name file))
        (message "Aborting"))))

  ;; (global-set-key (kbd "C-x f") 'counsel-recentf)
  (global-set-key (kbd "C-x f") 'my/recentf-open)

  ;; :bind (:map ivy-minibuffer-map
  ;;             ("RET" . ivy-alt-done))
  )


;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  ;; (projectile-register-project-type 'go '("Gopkg.toml" "go.mod"))
  ;; (projectile-register-project-type 'rust '("Cargo.toml"))
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
                                              (cl-dolist (deny '("\\.git" "\\.rustup" "\\.cargo" "go/pkg"))
                                                (when (string-match-p deny project-root)
                                                  (cl-return t)))))
  (projectile-mode +1)
  )

(use-package smex
  :config
  (smex-initialize)
  (setq smex-save-file (concat my/ignore-directory "smex-items")))


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
         ("<f11>" . treemacs)
         ("M-0" . treemacs-select-window))
  :config
  (progn
    (setq treemacs-persist-file (concat my/ignore-directory "treemacs-persist"))
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-projectile)
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons")
  )

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
