;; opam install tuareg merlin utop

;; https://github.com/ocaml/merlin/wiki/emacs-from-scratch
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (load "tuareg-site-file")
    ;; (load "merlin")
    (autoload 'merlin-mode "merlin" nil t nil)
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (setq tuareg-indent-align-with-first-arg t)
    (setq tuareg-match-patterns-aligned t)

    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq utop-command "opam config exec -- utop -emacs")
    ;; utop-minor cause merlin docstr didn't showup
    ;; (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    ))
