;;;
;;; scheme for sicp
;;;

(add-hook 'inferior-scheme-mode-hook 'enable-paredit-mode)

;; git clone http://git.sv.gnu.org/r/geiser.git
;; (let ((geiser-repo-dir "~/.emacs.d/vendor/geiser"))
;;   (when (file-exists-p geiser-repo-dir)
;;     (load-file (concat geiser-repo-dir "/elisp/geiser.el"))
;;     (require 'geiser)))
