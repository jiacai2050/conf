;; -*- lexical-binding: t -*-

(toggle-debug-on-error)
(load-file (expand-file-name "early-init.el" user-emacs-directory))

(setq package-user-dir "~/tt/miniemacs-elpa"
      package-check-signature nil)

(require 'package)
(package-initialize)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/no-littering")
  (setq auto-save-file-name-transforms '((".*" "~/tt/miniemacs-var" t))
        no-littering-var-directory (expand-file-name "~/tt/miniemacs-var")
        no-littering-etc-directory (expand-file-name "~/tt/miniemacs-etc")
        my/autoloads-dir (expand-file-name "~/tt/miniemacs-var/autoloads"))

  (unless (file-exists-p my/autoloads-dir)
    (make-directory my/autoloads-dir t))

  (unless (file-exists-p no-littering-etc-directory)
    (make-directory no-littering-etc-directory t))
  (require 'no-littering))

;; UI improve.
(eval-and-compile
  ;; No cursor blinking, it's distracting
  (blink-cursor-mode 0)
  (when (display-graphic-p)
    (set-frame-font "SF Mono-16" t t))
  (setq-default cursor-type 't))

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/company-mode")
  (require 'company)
  (global-company-mode t)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay .3
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-dabbrev-downcase nil
        company-show-numbers t
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-gtags company-etags company-keywords)
                           (company-files)))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-i") 'company-complete-selection))

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/swiper")
  (require 'counsel)
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-height 15
        ivy-extra-directories '("./")
        counsel-switch-buffer-preview-virtual-buffers nil)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "C-x f") 'counsel-switch-buffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)

  (ivy-mode 1))

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/avy")
  (setq avy-all-windows nil
        avy-keys (number-sequence ?a ?z))
  (global-set-key (kbd "C-C SPC") 'avy-goto-word-1))

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/evil")
  (setq evil-respect-visual-line-mode t
        evil-move-beyond-eol t)

  (require 'evil)
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
  (my/evil-keymap)
  (require 'dired)
  (evil-make-overriding-map dired-mode-map 'normal)
  )

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/evil-leader")
  (require 'evil-leader)
  (setq evil-leader/leader ","
        evil-leader/no-prefix-mode-rx '("dired.*")
        evil-leader/in-all-states t)

  (defun my/exec-shell-on-buffer (shell-command-text)
    (interactive "MShell command: ")
    (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))

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
    "s" 'counsel-git-grep
    "d" 'dired
    "f" 'counsel-find-file
    "k" 'kill-buffer
    "l" 'my/exec-shell-on-buffer

    "z" 'my/toggle-evil-emacs-mode
    "x" 'counsel-rg
    "c" 'compile
    "," 'my/insert-comma

    "SPC" 'avy-goto-word-1
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3)

  (global-evil-leader-mode 1)
  (evil-mode 1)
  )
