;; Simple Emacs plugins Python development

;; NOTE: Ensure that you have installed the jedi and pyflakes dependencies
;;
;; These are: 
;;   python PIP
;;   pip install jedi
;;   pip install --upgrade pyflakes

(simple-emacs-package-install 'jedi)
(simple-emacs-package-install 'flymake-python-pyflakes)
(simple-emacs-package-install 'pyvenv)
(simple-emacs-package-install 'python-environment)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(defun simple-python-hook ()
  (add-commented-annotations)
  (auto-complete-mode)
  (jedi:ac-setup)
  (linum-mode 1))

(add-hook 'python-mode-hook 'simple-python-hook)

(provide 'simple-emacs-python-devel)
