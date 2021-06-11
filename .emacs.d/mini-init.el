;; -*- lexical-binding: t -*-

(toggle-debug-on-error)
(load-file (expand-file-name "early-init.el" user-emacs-directory))

(setq package-user-dir "/tmp/elpa"
      package-check-signature nil)

(require 'package)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(use-package no-littering
  :load-path "~/.emacs.d/vendor/no-littering"
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        my/max-semantic-version "999.999.999"
        no-littering-var-directory (expand-file-name "~/tt/")
        no-littering-etc-directory (expand-file-name "~/tt/"))

  ;; (my/generate-autoloads 'company my/max-semantic-version
  ;;                        "~/.emacs.d/vendor/company-mode")
  (defun my/generate-autoloads (pkg-name version &rest dirs)
    (setq generated-autoload-file (no-littering-expand-var-file-name (format "%s-autoloads.el" pkg-name)))
    (unless (file-exists-p generated-autoload-file)
      (apply 'update-directory-autoloads dirs))
    (load-file generated-autoload-file)
    ;; push PACKAGE to package-alist so that package-installed-p can find it.
    (push (cons pkg-name (list (package-desc-create
                                :name pkg-name
                                :dir no-littering-var-directory
                                :version (version-to-list version))))
          package-alist)))

(use-package frame
  :ensure nil
  :config
  ;; No cursor blinking, it's distracting
  (blink-cursor-mode 0)
  (when (display-graphic-p)
    (set-frame-font "SF Mono-16" t t))
  (setq-default cursor-type 't))

(use-package company
  :load-path "~/.emacs.d/vendor/company-mode"
  :commands (global-company-mode)
  :init
  (global-company-mode t)

  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay .3
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-dabbrev-downcase nil
        company-show-numbers t
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-gtags company-etags company-keywords)
                           (company-files)))

  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-i" . company-complete-selection)))

(use-package counsel
  :load-path "~/.emacs.d/vendor/swiper"
  :commands (ivy-mode)
  :init
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)))
  (ivy-mode 1)
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
  :load-path ("~/.emacs.d/vendor/swiper" "~/.emacs.d/vendor/avy")
  :custom ((avy-all-windows nil)
           (avy-keys (number-sequence ?a ?z)))
  :bind (("C-x SPC" . avy-goto-char)
         ("C-c C-l" . avy-goto-line)
         ("C-C SPC" . avy-goto-word-1)))

(use-package evil
  :load-path "~/.emacs.d/vendor/evil"
  :hook ((evil-mode . my/evil-keymap))
  :custom ((evil-respect-visual-line-mode t)
           (evil-move-beyond-eol t))
  :commands (evil-mode evil-make-overriding-map evil-make-intercept-map)
  :init
  (require 'evil)
  (evil-mode 1)
  (defun my/evil-keymap ()
    (dolist (binding '(("SPC" . evil-scroll-page-down)
                       ("DEL" . evil-scroll-page-up)
                       ("C-e" . evil-end-of-line)
                       ("C-y" . yank)
                       ("C-d" . delete-char)
                       ("C-t" . transpose-chars)
                       ("C-o" . open-line)
                       ("M-." . xref-find-definitions)
                       ("M-," . xref-pop-marker-stack)
                       ("RET" . xref-goto-xref)
                       ("M-;" . comment-dwim-2)
                       ("C-M-b" . backward-sexp)
                       ("C-M-f" . forward-sexp)
                       ("q" . quit-window)))
      (define-key evil-normal-state-map (kbd (car binding)) (cdr binding)))

    (dolist (binding '(("SPC" . evil-scroll-page-down)
                       ("DEL" . evil-scroll-page-up)
                       ("C-e" . end-of-visual-line)
                       ("C-a" . beginning-of-visual-line)))
      (define-key evil-motion-state-map (kbd (car binding)) (cdr binding)))

    (dolist (binding '(("C-y" . yank)
                       ("C-w" . kill-region)
                       ("C-e" . end-of-visual-line)
                       ("C-a" . beginning-of-visual-line)
                       ("C-k" . kill-visual-line)
                       ("C-d" . delete-char)
                       ("C-v" . scroll-up-command)
                       ("C-t" . transpose-chars)
                       ("C-o" . open-line)
                       ("C-n" . next-line)
                       ("C-p" . previous-line)))
      (define-key evil-insert-state-map (kbd (car binding)) (cdr binding))))

  :config
  (require 'dired)
  (evil-make-overriding-map dired-mode-map 'normal))

(use-package evil-leader
  :load-path "~/.emacs.d/vendor/evil-leader"
  :init
  (require 'evil-leader)
  (global-evil-leader-mode)
  (defun my/exec-shell-on-buffer (shell-command-text)
    (interactive "MShell command: ")
    (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))

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
    "r" 'counsel-switch-buffer

    "a" 'swiper-isearch
    "s" 'my/search-command
    "d" 'dired
    "f" 'counsel-find-file
    "k" 'kill-buffer

    "z" 'my/toggle-evil-emacs-mode
    "x" 'counsel-rg
    "c" 'compile
    "v" 'counsel-org-capture
    "b" 'counsel-bookmark
    "," 'my/insert-comma

    "SPC" 'avy-goto-word-1
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4))
