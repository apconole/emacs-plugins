;;; Simple development goodies

(require 'iedit)
(require 'projectile)
(require 'yasnippet)
(require 'butler) ;; need to check which butler is included
(require 'flymake)

;; I use the following for ALL development buffers
(require 'impatient-mode)

;; set up yasnippet custom dirs
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))

;; append yasnippet as a source for auto-complete
(setq ac-source (append '(ac-source-yasnippet) ac-sources))

;; do autoloads
(when simple-emacs-autoload-all-dev
  (projectile-global-mode)
  (ac-config-default)
  (yas-global-mode 1))

;; Support for the ielm autocomplete
(defun simple-emacs-ielm-hook ()
  "Enables additional goodies for ielm mode"
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffer))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1)
  (linum-mode 1))

(add-hook 'ielm-mode-hook 'simple-emacs-ielm-hook)
	
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)


(provide 'simple-emacs-devel)
