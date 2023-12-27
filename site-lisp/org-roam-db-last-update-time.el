;;; org-roam-db-sync-optimization.el

;;; Commentary:
;;
;; This Emacs Lisp file optimizes the Org-roam database synchronization process by introducing
;; a mechanism to skip unnecessary synchronization when the database file has not been modified
;; since the last update. The timestamp of the last Org-roam database update is stored in an
;; external file.

;;; Code:

(defcustom org-roam-db-last-update-file "~/.emacs.d/org-roam-db-last-update-time"
  "File to store the timestamp of the last Org-roam database update."
  :type 'file
  :group 'org-roam)

(defvar org-roam-db-last-update-time nil
  "Timestamp of the last Org-roam database update.")

(defun org-roam-db-load-last-update-time ()
  "Load the timestamp of the last Org-roam database update from file.
If the file is not readable or does not exist, the timestamp remains nil."
  (when (file-readable-p org-roam-db-last-update-file)
    (setq org-roam-db-last-update-time
          (with-temp-buffer
            (insert-file-contents org-roam-db-last-update-file)
            (read (current-buffer))))))

(defun org-roam-db-save-last-update-time ()
  "Save the timestamp of the last Org-roam database update to file."
  (with-temp-buffer
    (prin1 org-roam-db-last-update-time (current-buffer))
    (write-region (point-min) (point-max) org-roam-db-last-update-file)))

(defun org-roam-db-update-time ()
  "Update the timestamp of the last Org-roam database update.
This function sets the timestamp to the current time and saves it to the external file."
  (setq org-roam-db-last-update-time (current-time))
  (org-roam-db-save-last-update-time))

(defun org-roam-db-sync-advice (orig-fun &rest args)
  "Advice function for org-roam-db-sync to check if syncing is necessary.
This advice checks whether the Org-roam database file has been modified since the last update.
If the file has been modified or the last update time is nil, it calls the original function (`org-roam-db-sync`),
updates the timestamp, and saves it to the external file."
  (let ((db-file-modified-time (nth 5 (file-attributes org-roam-db-location))))
    (when (or (null org-roam-db-last-update-time)
              (time-less-p org-roam-db-last-update-time db-file-modified-time))
      ;; Call the original function to perform synchronization
      (apply orig-fun args)
      ;; Update and save the timestamp
      (org-roam-db-update-time))))

;;; Initialization:

;; Load the last update time when Emacs starts
(org-roam-db-load-last-update-time)

;; Advising org-roam-db-sync
(advice-add 'org-roam-db-sync :around #'org-roam-db-sync-advice)

;; Save the last update time when Emacs is about to exit
(add-hook 'kill-emacs-hook 'org-roam-db-save-last-update-time)

(provide 'org-roam-db-sync-optimization)

;;; org-roam-db-sync-optimization.el ends here
