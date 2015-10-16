;; Sets up the emacs plugin list

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

(defcustom simple-emacs-multiple-smtp nil
  "Use multiple email accounts for smtp mail"
  :type 'boolean
  :group 'simple-emacs-plugins)

;; fix up 
(setq gc-cons-threshold 20000000)

;; basic stuff I just use a lot

(require 'package)
(package-refresh-contents)
(package-initialize)

(mapc (lambda(p) (push p package-archives))
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")))


(defun simple-emacs-package-install-no-require (pkgid)
  "Detects whether or not a package is installed, and if not, installs it"
  (if (not (package-installed-p pkgid))
      (progn
        (package-refresh-contents)
        (package-install pkgid)))
  )

(defun simple-emacs-package-install (pkgid)
  "Detects whether or not a package is installed, and if not, installs it, then
   loads it."
  (simple-emacs-package-install-no-require pkgid)
  (require pkgid))

(package-refresh-contents)

(simple-emacs-package-install 'cl)
(simple-emacs-package-install 's)
(simple-emacs-package-install 'helm)
(simple-emacs-package-install 'multiple-cursors)
(simple-emacs-package-install 'auto-complete)
(simple-emacs-package-install 'magit)
(simple-emacs-package-install 'git-timemachine)
(simple-emacs-package-install 'git-gutter)
(simple-emacs-package-install 'expand-region)
(simple-emacs-package-install 'smartparens)
(simple-emacs-package-install 'sx)
(simple-emacs-package-install-no-require 'solarized-theme)

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

(add-hook 'emacs-lisp-mode-hook (lambda () (linum-mode 1)))

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

;; Magit-blame in other window of current buffer
;; Disabled - Magit 2.2.0 no longer supports this
;; (defun magit-blame-other-window ()
;;   "Opens a new window from the current buffer filename and runs magit-blame on 
;;    it"
;;   (interactive)

;;   (setq buffer-name (concat (generate-new-buffer-name "*Magit Blame Mode: ") (buffer-file-name) (quote "*")))
;;   (setq previous-buffer-file-name (buffer-file-name))
;;   (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
;;   (setq buffer-file-name previous-buffer-file-name)
;;   (goto-char (point-min))
;;   (magit-blame)
;;   (local-set-key (kbd "q") 'kill-this-buffer))

(global-set-key (kbd "C-x g b") 'magit-blame-popup)

;; Git timemachine
(defun git-timemachine-other-window ()
  "Opens a new window from the current buffer filename and runs git-timemachine
   on it"
  (interactive)

  (setq buffer-name (generate-new-buffer-name "*Timemachine Mode*"))
  (setq previous-buffer-file-name (buffer-file-name))
  (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
  (setq buffer-file-name previous-buffer-file-name)
  (goto-char (point-min))
  (git-timemachine)
  (local-set-key (kbd "q") 'kill-this-buffer))

(global-set-key (kbd "C-x g t") 'git-timemachine-other-window)

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

;;(eval-after-load 'info
;;  '(progn (info-initialize)
;;          (add-to-list 'Info-directory-list (concat simple-emacs-plugins-dir "/magit"))))

(simple-emacs-package-install 'flymake)
(simple-emacs-package-install-no-require 'flymake-cursor)

(eval-after-load 'flymake '(require 'flymake-cursor))

(smartparens-global-mode t)

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

;;; Simple multi-smtp setup
(defun simple-emacs-multi-mail ()
  (if simple-emacs-multiple-smtp
      (save-excursion (loop with from = (save-restriction (message-narrow-to-headers)
                                                          (message-fetch-field "from"))
                            for (addr fname server port) in smtp-accounts
                            when (or (string-match addr from) (string-match (format "%s <%s>" fname addr) from))
                            do 
                            (setq user-mail-address addr
                                  user-full-name fname
                                  smtpmail-smtp-server server
                                  smtpmail-smtp-service port
                                  )
                            ))))

(defadvice smtpmail-via-smtp 
  (before change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer (simple-emacs-multi-mail)))

(provide 'simple-emacs-plugins-load)
