;; Web development junk

(require 'js2-mode)
(require 'web-mode)
(require 'impatient-mode)
(require 'js2-refactor)
(require 'nodejs-repl)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; set up js2r mode with C-c C-m <chord>
(js2r-add-keybindings-with-prefix "C-c C-m")

(setq js2-highlight-level 3)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js-mode-hook (lambda () (flymake-mode t)))

(define-key nodejs-repl-mode-map (kbd "C-x C-e") 'nodejs-repl-execute)

(eval-after-load "web-mode" '(impatient-mode))
(eval-after-load "web-mode" '(linum-mode 1))

(provide 'simple-emacs-web-devel)
