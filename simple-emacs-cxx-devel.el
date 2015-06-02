;;;;;;; Simple Emacs C++ development

(require 'auto-complete-clang)

(defcustom simple-emacs-plugins-gnu-gtags-binary "/usr/bin/gtags"
  "Path to GNU global"
  :type 'string
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-bundled-global f
  "Wether to use the bundled version of GNU Global"
  :type 'boolean
  :group 'simple-emacs-plugins)

;; allow to shut off the bundled gnu global - just in case
(if 
    (and (member
          (expand-file-name (concat simple-emacs-plugins-dir "/global"))
          load-path)
         (not simple-emacs-bundled-global))
    (delete (expand-file-name (concat simple-emacs-plugins-dir "/global"))
            load-path))

(require 'gtags)

(defcustom simple-emacs-plugins-gnu-global-binary "/usr/bin/global"
  "Path to GNU global"
  :type 'string
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-plugins-auto-run-gtags t
  "Whether to automatically run gtags / global update on file save"
  :type 'boolean
  :group 'simple-emacs-plugins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-tags building for C/C++

;; first, check to see if a tags directory exists...
;; (if so, the command will eventually be global -u)
(defun global-tags-root-dir ()
  "Returns GTAGS directory, or nil"
  (with-temp-buffer
    (if (zerop (call-process simple-emacs-plugins-gnu-global-binary nil t nil "-pr"))
	(buffer-substring (point-min) (1- (point-max)))
      nil)))

;; If we need to update, use global mode
(defun global-run-update ()
  "Executes a global -u"
  (call-process simple-emacs-plugins-gnu-global-binary nil nil nil "-u"))

;; If there isn't a tags, find the root .git -OR- use current directory
(defun global-do-initialize ()
  "Goes to the top level .git area and runs global -c"
  (let* ((gtags-default-directory (or (upward-find-file ".git") "."))
	 (global-initialize-command (concat "cd " gtags-default-directory " && " simple-emacs-plugins-gnu-gtags-binary)))
    (shell-command global-initialize-command)))

;; The hook to run (only in c/c++ mode)
(defun global-run-tags-automatic() 
  "Either updates tags which already exist, OR does a creation at the toplevel .git directory"
  (when (and simple-emacs-plugins-auto-run-gtags (member major-mode '(c++-mode c-mode)))
    (if (global-tags-root-dir) 
	(global-run-update)
      (global-do-initialize))))

(defun compile-next-makefile (flags)
  (interactive (list (read-string "Additional make flags: ")))
  (let* ((default-directory (or (upward-find-file "Makefile") "."))
	 (compile-command (concat "cd " default-directory " && "
				  (concat compile-command flags))))
    (compile compile-command)))

(setq gdb-many-windows t
      gdb-show-main t)

(setq gtags-auto-update t)

(defun simple-emacs-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+)

  (setq c++-tab-always-indent t) ;; pressing the 'tab' key always indents
  (setq c-basic-offset 4) ;; 4 space
  (setq c-indent-level 4) ;; default is 2
  
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  
  (setq ac-sources (append '(ac-source-clang) ac-sources))
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

  (flymake-mode 1)
  (linum-mode 1)
  (gtags-mode 1)
  (add-commented-annotations)
  (define-key c-mode-base-map (kbd "C-c C-l") (lambda () (interactive) (call-interactively 'compile-next-makefile)))

  (define-key c-mode-base-map (kbd "M-.") 'gtags-find-tag)
  (define-key c-mode-base-map (kbd "M-,") 'gtags-find-tag-from-here)
  (define-key c-mode-base-map (kbd "M-*") 'gtags-find-pattern))

(add-hook 'c-mode-common-hook 'simple-emacs-c-mode-hook)

(add-hook 'after-save-hook 'global-run-tags-automatic)

;; don't enable this until I know what's going on
;; (require 'doxymacs)
;; (defun simple-emacs-doxymacs-font-lock-hook ()
;;   (when (member major-mode '(c++-mode c-mode))
;;     (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'simple-emacs-doxymacs-font-lock-hook)

(provide 'simple-emacs-cxx-devel)
