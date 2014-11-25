;; Sets up the emacs plugin list

(defcustom simple-emacs-plugins-dir (concat user-emacs-directory "plugins")
  "Directory which contains all of the various plugins to load"
  :type 'string
  :group 'simple-emacs-plugins)

(let ((default-directory simple-emacs-plugins-dir))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (normal-top-level-add-subdirs-to-load-path))
	 load-path)))

;; for some reason, multiple-cursors.el isn't loaded??
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/multiple-cursors.el"))
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/expand-region.el"))
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/s.el"))
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/dash.el"))
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/pkg-info.el"))
(add-to-list 'load-path (concat simple-emacs-plugins-dir "/js2-refactor.el"))

(defcustom simple-emacs-autoload-all-dev t
  "Auto-enable YASnippets, auto-complete, and projectile global mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

;; basic stuff I just use a lot

(require 'cl)
(require 'helm)
(require 'multiple-cursors)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'magit)
(require 'expand-region)
(require 'smartparens-config)

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))



;; simple extention functions:
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first insance of FILE starting from the 
current directory."
  (let ((root (expand-file-name "/"))) 
    (expand-file-name file
		      (loop
		       for d = default-directory then (expand-file-name ".." d)
		       if (file-exists-p (expand-file-name file d))
		       return d
		       if (equal d root)
		       return nil))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
		    (if startdir startdir ".")))
	(found nil) ; found is set as a flag to leave loop if we find it
	(top nil))  ; top is set when we get
        ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
	    (setq top t))
      
      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
	    (setq found t)
	; If not, move up a directory
	(setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found (concat dirname "/") nil)))

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list (concat simple-emacs-plugins-dir "/magit"))))

(require 'flymake)

(eval-after-load 'flymake '(require 'flymake-cursor))

(smartparens-global-mode t)

;; good defaults, I think
(setq-default indent-tabs-mode nil)
(setq visibile-bell t
      require-final-newline t
      apropos-do-all t)

(require 'simple-emacs-devel)
(require 'simple-emacs-cxx-devel)
(require 'simple-emacs-python-devel)
(require 'simple-emacs-web-devel)

(require 'simple-emacs-social)

;; global magit status mode
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-=") 'er/expand-region)

(provide 'simple-emacs-plugins-load)
