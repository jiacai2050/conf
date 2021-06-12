;;; early-init.el -*- lexical-binding: t; -*-

(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . maximized)
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
      source-directory (expand-file-name "~/code/misc/emacs")
      package-enable-at-startup nil
      package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("marmalada" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

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

(defun my/buffer-indent ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)))

;; hack package.el, so it can find vendored packages
(defconst my/vendored-packages '(use-package company with-editor dash dash-functional
                                  transient ht s f spinner lv hydra request
                                  unicode-escape))

(defun my/package-installed-p (fn package &optional min-version)
  (or
   (member package my/vendored-packages)
   (funcall fn package min-version)))

(advice-add 'package-installed-p :around
            'my/package-installed-p)

(defun my/package-activate (fn package &optional force)
  (or (member package my/vendored-packages)
      (funcall fn package force)))

(advice-add 'package-activate :around
            'my/package-activate)
