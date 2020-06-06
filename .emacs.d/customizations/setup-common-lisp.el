    ;; https://common-lisp.net/project/slime/doc/html/Installation.html#Installation

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))
;; (require 'slime-autoloads)
