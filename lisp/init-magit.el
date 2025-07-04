;; -*- lexical-binding: t -*-
(use-package magit
  :straight (:repo "magit/magit"
		   :files ("lisp/magit*.el" "lisp/git-rebase.el" "lisp/git-commit.el"
			   "docs/magit.texi" "docs/AUTHORS.md"
			   "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
			   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el")
			   "magit-pkg.el"))
  :bind (("C-x g" . magit))
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (magit-auto-revert-mode t)
  (setq auto-revert-use-notify nil
        magit-branch-read-upstream-first 'fallback)
  (defun magit-submodule-remove+ ()
    (interactive)
    (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil)))

(use-package forge
             :straight t
  :after (magit)
  :defines forge-topic-list-columns
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :init
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil))))

(use-package diff-hl
  ;; :disabled
  :straight t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (after-init . global-diff-hl-mode))
  :config
  ;; makes my emacs freeze
  (setq diff-hl-update-async nil))
(use-package blamer
  :straight (:host github :repo "artawower/blamer.el" :branch "feature/margin-overlays")
  :bind (("s-i" . blamer-show-commit-info)
	 ("C-c i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 0)
  (blamer-entire-formatter "%s")
  :custom-face
  (blamer-face ((t :family "TriplicateT4c Nerd Font"
		   :foreground "#7a88cf"
                   ;; :background nil
                   :height 110
                   :italic t)))
  :init
  (global-blamer-mode 1)
  :config
  (setq blamer-type 'margin-overlay)
  )

;;; gptel auto commit message
;; (defconst gptel-commit-prompt
;;       "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

;; If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

;; Don't repeat information from the subject line in the message body.

;; Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

;; Follow good Git style:

;; - Separate the subject from the body with a blank line
;; - Try to limit the subject line to 50 characters
;; - Capitalize the subject line
;; - Do not end the subject line with any punctuation
;; - Use the imperative mood in the subject line
;; - Wrap the body at 72 characters
;; - Keep the body short and concise (omit it entirely if not useful)")
(defconst gptel-commit-prompt
  "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

(use-package magit
  :after gptel
  :commands (gptel-commit)
  :init
  (defun gptel-commit()
    "Generate commit message with gptel and insert it into the buffer."
    (interactive)
    (let* ((lines (magit-git-lines "diff" "--cached"))
           (changes (string-join lines "\n")))
      (gptel-request changes :system gptel-commit-prompt))))

;;; smerge accept all
(use-package smerge
  :config
  (defun smerge-resolve-all-in-file-to (to-keep)
    "Resolves all conflicts inside a file in preference of TO-KEEP

TO-KEEP decides which part to keep and is one of `upper',
`lower', `base'"
    (interactive
     (list (completing-read "Keeping (upper, base, lower): "
                          '(upper base lower))))
    (let ((resolve-func
           (pcase to-keep
           ("upper" 'smerge-keep-upper)
           ("base"  'smerge-keep-base)
           ("lower" 'smerge-keep-lower)
           (_ (error "Unknown resolution argument!"))))
          (num-chars-bfore (point-max)))
      (save-excursion
        (goto-char (point-min))
        (while (ignore-errors (not (smerge-next)))
          (funcall resolve-func)))
      (when (= num-chars-bfore (point-max))
        (message "No conflicts were found")))))

(provide 'init-magit)
