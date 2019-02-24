(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(defun my/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

;; https://stackoverflow.com/a/47850858/2163429
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "/tmp")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

(use-package ox-gfm
  :after org)

(use-package org
  :bind (:map org-mode-map
              ("C-c s" . org-table-sort-lines)
              ("C-c C-c" . org-toggle-inline-images))
  :init
  (setq org-src-tab-acts-natively t
        ;; 代码区域禁用第一层缩进 https://emacs.stackexchange.com/a/18892/16450
        org-src-preserve-indentation t
        org-log-done 'time
        org-startup-folded "showall"
        org-startup-indented t
        org-image-actual-width nil
        ;; terminal emacs can't display those lovely images :-(
        org-startup-with-inline-images t)
  ;; markdown export require emacs 25 https://stackoverflow.com/a/33033533/2163429
  (require 'ox-md nil t)

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.6 :bold t))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.4 :bold t))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2 :bold t))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0 :bold t))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0 :bold t))))
   )
  ;; #+LaTeX_HEADER: \usepackage{CJK}
  ;; #+LaTeX_HEADER: \begin{CJK}{UTF8}{gbsn}
  (add-to-list 'org-latex-packages-alist '("" "CJKutf8" t))

  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

  (setq org-publish-project-alist
        '(("org-notes"
           :base-directory "~/study-note/"
           :base-extension "org"
           :publishing-directory "~/Documents/public_notes"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)

          ("org-static"
           :base-directory "~/study-note/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Documents/public_notes"
           :recursive t
           :publishing-function org-publish-attachment))))

(when (display-graphic-p)
  (cnfonts-enable)
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  (global-set-key (kbd "<f5>") 'cnfonts-increase-fontsize)
  (global-set-key (kbd "<f6>") 'cnfonts-decrease-fontsize))
