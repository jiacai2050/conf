;;; -*- lexical-binding: t; -*-

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

(when (and (memq window-system '(mac ns x))
           (display-graphic-p))
  ;; emacs run inside terminal will inherit env from shell
  (use-package exec-path-from-shell
    :custom ((exec-path-from-shell-arguments '("-l"))
             (exec-path-from-shell-variables '("GOPROXY" "GOPATH" "PATH" "MY_THEME")))
    :config
    (exec-path-from-shell-initialize))
  )

(use-package company-native-complete
  :init
  (defun my/set-shell-backends ()
	(setq-local company-backends '(company-native-complete)))
  :hook ((shell-mode . my/set-shell-backends))
  :config
  (native-complete-setup-bash)
)

(use-package aweshell
  :load-path "~/.emacs.d/vendor/aweshell"
  :bind (("<f8>" . aweshell-dedicated-toggle)
         ("<f9>" . aweshell-toggle))
  :config
  (when (display-graphic-p)
    (setq aweshell-use-exec-path-from-shell nil))
  (setq aweshell-auto-suggestion-p nil)
  (setq-local company-backends '(company-capf)
              pcomplete-cycle-completions nil)
  (setq eshell-prompt-function
        (lambda ()
          (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
          (concat "["
                  (user-login-name)
                  "@"
                  (system-name)
                  " "
                  (format-time-string "%H:%M" (current-time))
                  " "
                  (abbreviate-file-name (eshell/pwd))

                  "] "
                  (when (epe-git-p)
                    (concat "("
                            (epe-git-branch)
                            (epe-git-dirty)
                            (epe-git-untracked)
                            (let ((unpushed (epe-git-unpushed-number)))
                              (unless (= unpushed 0)
                                (concat ":" (number-to-string unpushed))))
                            ")"))
                  (if (= (user-uid) 0) "# " "$ ")
                  "\n"))))
