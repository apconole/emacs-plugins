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

;; simple 'keywords' that are always relevant
(defun add-commented-annotations ()
  (font-lock-add-keywords
   nil '(("\\<@?\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\(:\\|(\\)\\)"
          1 font-lock-warning-face t))))

;;; From binchen.org
(require 'w3m)

(defun w3m-get-url-from-search-engine-alist (k l)
  (let (rlt)
    (if (listp l)
      (if (string= k (caar l))
          (setq rlt (nth 1 (car l)))
        (setq rlt (w3m-get-url-from-search-engine-alist k (cdr l)))))
    rlt))

(defun w3m-set-url-from-search-engine-alist (k l url)
    (if (listp l)
      (if (string= k (caar l))
          (setcdr (car l) (list url))
        (w3m-set-url-from-search-engine-alist k (cdr l) url))))

;; C-u S g RET <search term> RET in w3m
(setq w3m-search-engine-alist
      '(("g" "http://www.google.com.au/search?q=%s" utf-8)
        ;; stackoverflow search
        ("q" "http://www.google.com.au/search?q=%s+site:stackoverflow.com" utf-8)
        ;; elisp code search
        ("s" "http://www.google.com.au/search?q=%s+filetype:el"  utf-8)
        ;; wikipedia
        ("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
        ;; online dictionary
        ("d" "http://dictionary.reference.com/search?q=%s" utf-8)
        ;; javascript search on mozilla.org
        ("j" "http://www.google.com.au/search?q=%s+site:developer.mozilla.org" utf-8)))

(defun w3m-google-by-filetype ()
  (interactive)
  (unless (featurep 'w3m)
    (require 'w3m))
  (let ((thing (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol)))
        (old-url (w3m-get-url-from-search-engine-alist "s" w3m-search-engine-alist))
        new-url)
    (when buffer-file-name
      (setq new-url (replace-regexp-in-string
                     "filetype:.*"
                     (concat "filetype:" (file-name-extension buffer-file-name))
                     old-url))
      (w3m-set-url-from-search-engine-alist "s" w3m-search-engine-alist new-url))
    ;; change the url to search current file type
    (w3m-search "s" thing)
    ;; restore the default url
    (w3m-set-url-from-search-engine-alist "s" w3m-search-engine-alist old-url)))

(global-set-key (kbd "M-/") 'w3m-google-by-filetype)

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
