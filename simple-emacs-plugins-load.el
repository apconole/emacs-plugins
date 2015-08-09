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

;; Need explicit loading for the .el files
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

(defcustom simple-emacs-cxx-development t
  "Auto-enable cxx mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-python-development t
  "Auto-enable python development mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-web-development t
  "Auto-enable web development mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-ruby-development t
  "Auto-enable ruby development mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

(defcustom simple-emacs-social-mode t
  "Auto-enable social mode"
  :type 'boolean
  :group 'simple-emacs-plugins)

;; basic stuff I just use a lot

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'cl)
(require 'helm)
(require 'multiple-cursors)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'magit-autoloads)
(require 'git-timemachine)
(require 'git-gutter)
(require 'expand-region)
(require 'smartparens-config)
(require 'sx-load)

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; this is a bit flaky...
(global-git-gutter-mode +1)

(git-gutter:linum-setup)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x g n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x g s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x g r") 'git-gutter:revert-hunk)

(setq git-gutter:modified "="
      git-gutter:added-sign ">"
      git-gutter:deleted-sign "<"
      git-gutter:update-interval 2)

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
(setq gc-cons-threshold 20000000)

(setq inhibit-splash-screen t)         ; hide welcome screen
(setq inhibit-startup-message t)       ; and the welcome message
(setq inhibit-startup-screen t)        ; because emacs has 1000 variables...

(defun se:git-clone (repository &optional target)
  (interactive "sGit URL: \nsTarget Dir: ")
  (setq local_target "")
  (if target
      (setq local_target target))
  (shell-command (concat "git clone --recursive " repository " " target)))


;; uniquify
(require 'uniquify)
(require 'saveplace)

;; partial changes taken from better-defaults package

;; The following MUST be setq-default because they are buffer-local
(setq-default indent-tabs-mode nil
              uniquify-buffer-name-style 'forward
              save-place t)

;; The following are non-buffer local
(setq visibile-bell t
      require-final-newline t
      apropos-do-all t
      uniquify-separator ":"
      save-place-file (concat user-emacs-directory "visited"))

(require 'simple-emacs-devel)
(if simple-emacs-cxx-development (require 'simple-emacs-cxx-devel))
(if simple-emacs-python-development (require 'simple-emacs-python-devel))
(if simple-emacs-web-development (require 'simple-emacs-web-devel))
(if simple-emacs-social-mode (require 'simple-emacs-social))
(if simple-emacs-ruby-development (require 'simple-emacs-ruby-devel))

;; global magit status mode
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c g t") 'git-timemachine)

(provide 'simple-emacs-plugins-load)
