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
(simple-emacs-package-install 'elpy)

(defun simple-python-hook ()
  (add-commented-annotations)
  (auto-complete-mode)
  (jedi:ac-setup)
  ;; (linum-mode 1)
  (elpy-mode)
  (local-set-key (kbd "M-.") 'elpy-goto-definition)
  (flymake-python-pyflakes-load))

(add-hook 'python-mode-hook 'simple-python-hook)

(provide 'simple-emacs-python-devel)
