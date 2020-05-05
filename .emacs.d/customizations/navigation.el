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
  (projectile-mode)
  ;; (projectile-register-project-type 'go '("Gopkg.toml" "go.mod"))
  ;; (projectile-register-project-type 'rust '("Cargo.toml"))

  (setq projectile-switch-project-action #'projectile-find-file-dwim
        projectile-completion-system 'ido
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

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :config
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-everywhere t
        )
  (ido-mode t)
  (defun my/ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (let ((file (ido-completing-read "Find recent file: " (mapcar 'abbreviate-file-name recentf-list))))
      (if (find-file file)
          (message "Opening file %s" (abbreviate-file-name file))
        (message "Aborting"))))

  (global-set-key (kbd "C-x f") 'my/ido-recentf-open)
  )

;; (use-package ido-hacks
;;   :requires ido
;;   :config (ido-hacks-mode))

;; (use-package ido-ubiquitous
;;   :requires ido
;;   :config (ido-ubiquitous-mode))

;; (use-package flx-ido
;;   :requires ido
;;   :config (flx-ido-mode))

(use-package ido-vertical-mode
  :requires ido
  :config
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        ido-vertical-show-count t
        ido-vertical-disable-if-short nil)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background nil
                      :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background nil
                      :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground nil)
  )

(use-package smex
  :requires ido
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  )

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
