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

(defun my/helm-hide-minibuffer-maybe ()
  "Hide minibuffer contents in a Helm session.
   https://github.com/emacs-helm/helm/blob/353c84076d5489b6a4085537775992226f9d5156/helm.el#L4942"
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (helm-aif (and helm-display-header-line
                     (helm-attr 'persistent-help))
          (progn
            (overlay-put ov 'display
                         (truncate-string-to-width
                          (substitute-command-keys
                           (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
                                   (format "%s (keeping session)" it)))
                          (- (window-width) 1)))
            (overlay-put ov 'face 'helm-header))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color))))

      (setq cursor-type nil))))

(use-package helm
  ;; :init
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
  ;; (global-unset-key (kbd "C-x c"))
  :pin melpa-stable
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)

  (setq helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t
        helm-etags-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t
        ;; https://github.com/emacs-helm/helm/issues/1676
        helm-move-to-line-cycle-in-source nil
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        )

  
  (add-hook 'helm-minibuffer-set-up-hook 'my/helm-hide-minibuffer-maybe)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x i" . helm-imenu)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-buffers-list)
         ("C-x w" . helm-toggle-resplit-and-swap-windows)
         ;; ("C-SPC" . helm-dabbrev)
         ;; ("M-y" . helm-show-kill-ring)
         ))


(use-package helm-ls-git
  :after helm
  :ensure t
  :bind (("C-x C-d" . 'helm-browse-project)))

(use-package helm-descbinds
  :after helm
  :config (helm-descbinds-mode))

;; projectile everywhere!
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (projectile-register-project-type 'go '("Gopkg.toml" "go.mod"))

  (setq projectile-switch-project-action #'projectile-find-file-dwim
        projectile-completion-system 'helm
        ;; projectile-enable-caching t
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                   projectile-root-bottom-up
                                                   projectile-root-local)
        ))

(use-package helm-projectile
  :after (projectile helm)
  ;; :bind ("C-c f" . helm-projectile-find-file)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile-find-file))

;; https://github.com/senny/emacs.d/blob/83567797b14e483ae043b7fe57b3154ae9972b4c/init.el#L107
(use-package helm-ag
  :after helm-projectile
  ;; :bind ("C-c g g" . helm-projectile-ag)
  )


(use-package helm-gtags
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history))
  :hook ((dired-mode eshell-mode c-mode c++-mode asm-mode) . helm-gtags-mode))

(use-package sr-speedbar
  :config
  (setq speedbar-show-unknown-files t ;; show all files
        speedbar-use-images nil       ;; use text for buttons
        sr-speedbar-right-side nil    ;; put on left side
        sr-speedbar-width 30
        )
  :bind (("C-c s o" . sr-speedbar-toggle)
         ("C-c s w" . sr-speedbar-select-window)
         ("C-c s r" . sr-speedbar-refresh-toggle)))

;; (defun my/ido-recentf-open ()
;;   "Use `ido-completing-read' to find a recent file."
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; (global-set-key (kbd "C-x f") 'my/ido-recentf-open)

;; ;; ido-mode allows you to more easily navigate choices. For example,
;; ;; when you want to switch buffers, ido presents you with a list
;; ;; of buffers in the the mini-buffer. As you start to type a buffer's
;; ;; name, ido will narrow down the list of buffers to match the text
;; ;; you've typed in
;; ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; (ido-mode t)

;; ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
;; (setq ido-enable-flex-matching t)

;; ;; Turn this behavior off because it's annoying
;; (setq ido-use-filename-at-point nil)

;; ;; Don't try to match file across all "work" directories; only match files
;; ;; in the current directory displayed in the minibuffer
;; (setq ido-auto-merge-work-directories-length -1)

;; ;; Includes buffer names of recently open files, even if they're not
;; ;; open now
;; (setq ido-use-virtual-buffers t)

;; ;; This enables ido in all contexts where it could be useful, not just
;; ;; for selecting buffer and file names
;; (ido-ubiquitous-mode 1)

;; ;; Shows a list of buffers
;; (global-set-key (kbd "C-x C-b") 'ibuffer)


;; ;; Enhances M-x to allow easier execution of commands. Provides
;; ;; a filterable list of possible commands in the minibuffer
;; ;; http://www.emacswiki.org/emacs/Smex
;; (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)

