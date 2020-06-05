;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 150)

;; https://stackoverflow.com/a/950553/2163429
(global-visual-line-mode)
;; this doesn't work as expect
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)


(use-package ag
  :ensure-system-package (ag . "brew install the_silver_searcher")
  )

;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  ;; (projectile-register-project-type 'go '("Gopkg.toml" "go.mod"))
  ;; (projectile-register-project-type 'rust '("Cargo.toml"))

  (setq projectile-switch-project-action #'projectile-find-file-dwim
        projectile-completion-system 'ivy
        ;; projectile-enable-caching t
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                   projectile-root-bottom-up
                                                   projectile-root-local)
        ))

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

(use-package smex
  :requires ido
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

;; counsel ivy swiper
(use-package counsel
  :ensure t
  :config
  (ivy-mode +1)
  ;; (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (my/recentf-open . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)
                                )
        (setq ivy-extra-directories ())
        )

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

  (global-set-key (kbd "C-x f") 'my/recentf-open)
  ;; (global-set-key (kbd "C-x f") 'counsel-recentf)
  )

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
