;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(defvar my/ignore-directory (file-name-as-directory (expand-file-name "ignore" user-emacs-directory)))

(defmacro comment (&rest body)
  nil)

(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars)))

(setq gc-cons-threshold most-positive-fixnum
      ;; 1mb
      read-process-output-max (* 1024 1024)
      ;; copy from doom-emacs
      frame-inhibit-implied-resize t
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
