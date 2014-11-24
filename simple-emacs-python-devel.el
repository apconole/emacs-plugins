;; Simple Emacs plugins Python development

;; NOTE: Ensure that you have installed the jedi and pyflakes dependencies
;;
;; These are: 
;;   python PIP
;;   pip install jedi
;;   pip install --upgrade pyflakes

(require 'jedi)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(provide 'simple-emacs-python-devel)
