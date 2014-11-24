;; Web development junk

(require 'js2-mode)
(require 'web-mode)
(require 'impatient-mode)
(require 'js2-refactor)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" .web-mode))

; set up js2r mode with C-c C-m <chord>
(js2r-add-keybindings-with-prefix "C-c C-m")

(eval-after-load "web-mode" '(impatient-mode))

(provide 'simple-emacs-web-devel)
