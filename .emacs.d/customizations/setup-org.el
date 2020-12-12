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

(use-package htmlize
  :after org)

(use-package ob-http
  :after org)

(use-package org
  :bind (:map org-mode-map
         ("C-c SPC" . ace-jump-mode)
         ("C-c s" . org-table-sort-lines)
         ("C-c l" . org-store-link))
  :custom (org-default-notes-file (expand-file-name "~/Documents/notes.org"))
  :init
  ;; https://orgmode.org/worg/org-contrib/babel/languages.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (shell . t)
     (python . t)
     (makefile . t)
     (http . t)
     (clojure . t)
     (emacs-lisp . t)))
  (setq org-src-tab-acts-natively t
        ;; 代码区域禁用第一层缩进 https://emacs.stackexchange.com/a/18892/16450
        org-src-preserve-indentation t
        org-log-done 'time
        org-startup-folded "showall"
        org-startup-indented t
        org-image-actual-width nil
        org-export-with-sub-superscripts nil
        org-hide-emphasis-markers nil
        ;; terminal emacs can't display those lovely images :-(
        org-startup-with-inline-images t)
  ;; markdown export require emacs 25 https://stackoverflow.com/a/33033533/2163429
  (require 'ox-md nil t)
  (require 'org-tempo)
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

(use-package org-download
  :ensure-system-package (pngpaste . "brew install pngpaste")
  :bind (:map org-mode-map
              ("C-c v" . org-download-screenshot)
              ("C-c d" . org-download-delete))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "./img"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-image-org-width 600
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
  (setq org-download-annotate-function (lambda (link) "")))

;; https://emacs-china.org/t/topic/440
;; (use-package cnfonts
;;   :config
;;   (when (display-graphic-p)
;;     (cnfonts-enable)
;;     (setq cnfonts-profiles
;;           '("program" "org-mode" "read-book"))
;;     (global-set-key (kbd "<f5>") 'cnfonts-increase-fontsize)
;;     (global-set-key (kbd "<f6>") 'cnfonts-decrease-fontsize)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-table ((t (:family "M+ 1m")))))
;; (set-face-attribute 'org-table nil :family "Inconsolata")
