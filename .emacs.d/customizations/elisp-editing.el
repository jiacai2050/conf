;;; -*- lexical-binding: t; -*-

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
    ;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
    (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
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

  (defun Fuco1/lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))
  )

(use-package ielm
  :ensure nil
  :config
  (defun ielm/clear-repl ()
    "Clear current REPL buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ielm-send-input)))
  :bind (:map inferior-emacs-lisp-mode-map
         ("M-RET" . ielm-return)
         ("C-j" . ielm-return)
         ("RET" . electric-newline-and-maybe-indent)
         ("C-c l" . ielm/clear-repl)))
