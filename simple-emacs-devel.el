;;; Simple development goodies

(require 'iedit)
(require 'projectile)
(require 'yasnippet)
(require 'butler) ;; need to check which butler is included
(require 'flymake)
(require 'asn1-mode)

;; I use the following for ALL development buffers
(require 'impatient-mode)

;; set up yasnippet custom dirs
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))

;; append yasnippet as a source for auto-complete
(setq ac-source (append '(ac-source-yasnippet) ac-sources))

;; Disable the annoying flymake popup
(setq flymake-gui-warnings-enabled nil)

;; do autoloads
(when simple-emacs-autoload-all-dev
  (projectile-global-mode)
  (ac-config-default)
  (yas-global-mode 1))

;;; This comes from endlessparentheses.com blog post
;;; Credits to Michael Fogleman and Artur Malabarba
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; setup C-x n to narrow / widen
;; This is quite useful for developing in python/emacs
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(eval-after-load 'org-src
  '(define-key org-src-mode-map "\C-x\C-s" #'org-edit-src-exit))

;; Support for the ielm autocomplete
(defun simple-emacs-ielm-hook ()
  "Enables additional goodies for ielm mode"
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffer))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1)
  (linum-mode 1))

(add-hook 'ielm-mode-hook 'simple-emacs-ielm-hook)
	
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)

;; Set up the asn.1 binding
(add-to-list 'auto-mode-alist '("\\.asn1$" . asn1-mode))
(add-to-list 'auto-mode-alist '("\\.gdmo$" . asn1-mode))

(provide 'simple-emacs-devel)
