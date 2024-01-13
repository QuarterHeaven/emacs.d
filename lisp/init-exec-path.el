;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package exec-path-from-shell
;;   :straight t
;;   :init

;;   (with-eval-after-load 'exec-path-from-shell
;;     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;       (add-to-list 'exec-path-from-shell-variables var)))


;;   (when (or (memq window-system '(mac ns x pgtk))
;;             (unless (memq system-type '(ms-dos windows-nt))
;;               (daemonp)))
;;     (setq exec-path-from-shell-check-startup-files nil) ;
;;     (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
;;     (exec-path-from-shell-initialize)))
;; (defun set-exec-path-from-shell-PATH ()
;;       "This is particularly useful under Mac OS X and macOS."
;;       (interactive)
;;       (let ((path-from-shell (replace-regexp-in-string
;;                               "[ \t\n]*$" "" (shell-command-to-string
;;                                               "fish --login -c 'echo
;;                                               $PATH'"))))
;;         (setenv "PATH" path-from-shell)
;;         (setq exec-path (split-string path-from-shell
;; 				      path-separator))))
(if sys/macp
    (condition-case err
	(let ((path (with-temp-buffer
                      (insert-file-contents-literally "~/.path")
                      (buffer-string))))
	  (setenv "PATH" path)
	  (setq exec-path (append (parse-colon-path path) (list exec-directory))))
      (error (warn "%s" (error-message-string err))))

  (use-package exec-path-from-shell
    :disabled
    :straight t
    :commands exec-path-from-shell-initialize
    :if (not (memq system-type '(cygwin windows-nt)))
    :custom
    (exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
