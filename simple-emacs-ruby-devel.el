;; Simple Emacs plugins Ruby development

(defcustom simple-emacs-ruby-binary "/usr/bin/ruby"
  "The path to the ruby 1.9.2+ compatible interpreter"
  :type 'string
  :group 'simple-emacs-plugins)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-program simple-emacs-ruby-binary)

(provide 'simple-emacs-ruby-devel)
