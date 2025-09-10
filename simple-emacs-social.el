;; Social media stuff...

(simple-emacs-package-install 'jabber)
;;(simple-emacs-package-install 'twittering-mode)
;;(simple-emacs-package-install 'rcirc-notify)

(load "jabber-autoloads")

(provide 'simple-emacs-social)

(setq rcirc-log-flag 't)

;; rcirc reload (from the rcirc manual)
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))

(add-hook 'rcirc-mode-hook (lambda ()
                             (flyspell-mode 1)))

(add-to-list 'load-path (concat user-emacs-directory "plugins/org-contacts"))


(defun normal-message-insert-citation-line ()
  "Insert a simple citation line."
  (when message-reply-headers
    (insert "On " (mail-header-date message-reply-headers) ", " (mail-header-from message-reply-headers) " wrote:")
    (newline)
    (newline)))
;; need to set the gnus insert-citation variable
