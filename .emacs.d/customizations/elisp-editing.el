;;; -*- lexical-binding: t; -*-

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . my/elisp-company)
         (lisp-interaction-mode . my/elisp-company))
  :bind (:map emacs-lisp-mode-map
              ("C-c M-n" . macrostep-expand)
              ("C-c RET" . my/elisp-macroexpand)
         :map lisp-interaction-mode-map
              ("C-c M-n" . macrostep-expand)
              ("C-c RET" . my/elisp-macroexpand))
  :config
  (defun my/elisp-company ()
    (setq-local company-backends
                '((company-elisp company-dabbrev-code))))

  (defun my/elisp-macroexpand ()
    (interactive)
    (let* ((start (point))
           (exp (read (current-buffer)))
           ;; Compute it before, since it may signal errors.
           (new (macroexpand-1 exp)))
      (if (equal exp new)
          (message "Not a macro call, nothing to expand")
        (with-current-buffer (get-buffer-create "*elisp-macroexpand*")
          (let ((bf (current-buffer)))
            (view-mode -1)
            (erase-buffer)
            (pp new bf)
            (switch-to-buffer-other-window bf)
            (forward-line -100)
            (emacs-lisp-mode)
            (view-mode 1))))))
  )

(require 'ielm)

(defun ielm/clear-repl ()
  "Clear current REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ielm-send-input)))

(define-key inferior-emacs-lisp-mode-map
  (kbd "M-RET")
  #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
  (kbd "C-j")
  #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
  (kbd "RET")
  #'electric-newline-and-maybe-indent)

(define-key inferior-emacs-lisp-mode-map
  (kbd "<up>")
  #'previous-line)

(define-key inferior-emacs-lisp-mode-map
  (kbd "<down>")
  #'next-line)

(define-key inferior-emacs-lisp-mode-map
  (kbd "C-c C-q")
  #'ielm/clear-repl)
