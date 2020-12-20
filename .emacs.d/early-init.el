;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(defvar my/ignore-directory (file-name-as-directory (expand-file-name "ignore" user-emacs-directory)))
(defvar package-directory-list '())

;; add cask dependencies
(dolist (cask-dep '("~/.emacs.d/vendor/lsp-mode" "~/.emacs.d/vendor/lsp-treemacs"))
  (let ((dep-dir (format "%s/.cask/%s.%s/elpa" cask-dep emacs-major-version emacs-minor-version)))
    (add-to-list 'package-directory-list dep-dir)))

(dolist (f load-path)
  (when (and (stringp f)
             (equal (file-name-nondirectory f) "site-lisp"))
    (add-to-list 'package-directory-list (expand-file-name "elpa" f))))

(setq default-frame-alist '((fullscreen . maximized)
                            (tool-bar-lines . -1)
                            (menu-bar-lines . -1)
                            (font . "Hack-15")
                            (vertical-scroll-bars)))

(setq gc-cons-threshold most-positive-fixnum
      ;; 1mb
      read-process-output-max (* 1024 1024)
      load-prefer-newer t
      source-directory (expand-file-name "~/code/misc/emacs")
      )

;; http://akrl.sdf.org/
(defmacro my/operation-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 30s run the GC no matter what.
(defvar my/gc-timer
  (run-with-idle-timer 30 t
                       (lambda ()
                         (let ((inhibit-read-only t)
                               (gc-msg (format "Garbage Collector has run for %.06fsec"
                                               (my/operation-time (garbage-collect)))))
                           (with-current-buffer "*Messages*"
	                         (insert gc-msg "\n"))))))
