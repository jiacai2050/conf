;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


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
  :init
  (setq recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-max-menu-items 40
        recentf-max-saved-items 150)
  (recentf-mode +1)
  )

;; https://stackoverflow.com/a/950553/2163429
(global-visual-line-mode)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
  (global-set-key (kbd "C-c g g") 'counsel-git-grep)

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
  :requires ido
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))


(use-package ag
  :ensure-system-package (ag . "brew install the_silver_searcher"))

(use-package sr-speedbar
  :config
  (setq speedbar-show-unknown-files t ;; show all files
        speedbar-use-images nil       ;; use text for buttons
        sr-speedbar-right-side nil    ;; put on left side
        sr-speedbar-width 30
        )
  :bind (("<f11>" . sr-speedbar-toggle)
         ("C-c s w" . sr-speedbar-select-window)
         ("C-c s r" . sr-speedbar-refresh-toggle)))

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

(global-set-key (kbd "C-c b") 'my/switch-to-metadata-file)
