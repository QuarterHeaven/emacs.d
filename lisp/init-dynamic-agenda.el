;;* dynamic agenda https://github.com/brianmcgillion/doomd/blob/master/config.org
  ;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  ;; The 'roam-agenda' tag is used to tell vulpea that there is a todo item in this file
  (add-to-list 'org-tags-exclude-from-inheritance "roam-agenda")

  (require 'vulpea)

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag (&optional arg)
    "Update PROJECT tag in the current buffer."
    (interactive "P")
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "roam-agenda" tags))
            (setq tags (remove "roam-agenda" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun dynamic-agenda-files-advice (orig-val)
    (let ((roam-agenda-files (delete-dups (my/org-roam-list-notes-by-tag "roam-agenda"))))
      (cl-union orig-val roam-agenda-files :test #'equal)))

  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)

(provide 'init-dynamic-agenda)
