(use-package ruby-mode
  :mode ("\\.rake$"
         "\\.gemspec$"
         "\\.ru$"
         "\\.cap$"
         "\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file$"
         ;; "Rakefile$"
         ;; "Gemfile$"
         ;; "Capfile$"
         ;; "Vagrant$"
         ;; "Guardfile$"
         ))

(use-package robe
  :after ruby-mode
  :init
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (eval-after-load 'company
      '(push 'company-robe company-backends))
    ))

(use-package ruby-end)

(use-package inf-ruby)
