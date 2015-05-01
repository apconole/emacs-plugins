;; Web development junk

(require 'js2-mode)
(require 'web-mode)
(require 'impatient-mode)
(require 'js2-refactor)
(require 'ac-js2)
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

(add-hook 'js2-mode-hook '(lambda ()
                            (add-commented-annotations)
                            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
                            (local-set-key (kbd "C-x C-r") 'js-send-region)
                            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
                            (local-set-key (kbd "C-c b") 'js-send-buffer)))

(add-hook 'web-mode-hook '(lambda () (add-commented-annotations) (impatient-mode)))
(add-hook 'web-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'js-mode-hook '(lambda () (add-commented-annotations) (linum-mode 1)))

(add-to-list 'ac-modes 'web-mode)

(provide 'simple-emacs-web-devel)
