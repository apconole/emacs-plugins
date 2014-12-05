;; Simple Emacs plugins Ruby development

(defcustom simple-emacs-ruby-binary "/usr/bin/ruby"
  "The path to the ruby 1.9.2+ compatible interpreter"
  :type 'string
  :group 'simple-emacs-plugins)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-program simple-emacs-ruby-binary)
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s"
                   (upward-find-file "Gemfile")
                   (file-relative-name (buffer-file-name) (upward-find-file "Gemfile"))
                   ) t))

(defun rspec-compile-line ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s -l %s"
                   (upward-find-file "Gemfile")
                   (file-relative-name (buffer-file-name) (upward-find-file "Gemfile"))
                   (line-number-at-pos)
                   ) t))

(add-hook 'enh-ruby-mode-hook
          (lambda () 
            (local-set-key (kbd "C-c C-l") 'rspec-compile-file)
            (local-set-key (kbd "C-c l") 'rspec-compile-line)))

(provide 'simple-emacs-ruby-devel)
