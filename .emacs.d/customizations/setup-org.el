(setq org-log-done 'time)
(setq org-startup-folded "showall")
(setq org-startup-indented t)
;; markdown export require emacs 25 https://stackoverflow.com/a/33033533/2163429
(require 'ox-md nil t)
;; terminal emacs can't display those lovely images :-(
(setq org-startup-with-inline-images t)
;; #+LaTeX_HEADER: \usepackage{CJK}
;; #+LaTeX_HEADER: \begin{CJK}{UTF8}{gbsn}
(add-to-list 'org-latex-packages-alist '("" "CJKutf8" t))

(defun my/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-c s") #'org-table-sort-lines)))

(when (display-graphic-p)
  (cnfonts-enable)
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  (global-set-key (kbd "<f5>") 'cnfonts-increase-fontsize)
  (global-set-key (kbd "<f6>") 'cnfonts-decrease-fontsize))

(require 'ox-publish)
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
         :publishing-function org-publish-attachment)))

;; https://stackoverflow.com/a/47850858/2163429
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "/tmp")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)
