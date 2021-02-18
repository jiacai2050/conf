;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars))
      gc-cons-threshold most-positive-fixnum
      ;; 1mb
      read-process-output-max (* 1024 1024)
      ;; copy from doom-emacs
      frame-inhibit-implied-resize t
      load-prefer-newer t
      inhibit-startup-screen t
      ;; No need for ~ files when editing
      create-lockfiles nil
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html
      enable-local-variables :safe
      source-directory (expand-file-name "~/code/misc/emacs"))

(defmacro comment (&rest body)
  nil)

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

(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

(defun my/global-map-and-set-key (key command &optional prefix suffix)
  "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
   (my/map-key key)
   (global-set-key (kbd (concat prefix key suffix)) command))

(defconst MAC-P      (eq system-type 'darwin))
(defconst LINUX-P    (eq system-type 'gnu/linux))
(defconst WINDOWS-P  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst BSD-P      (eq system-type 'berkeley-unix))
