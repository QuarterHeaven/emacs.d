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
  (setq auto-revert-use-notify nil)
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
  :straight t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (after-init . global-diff-hl-mode))
  :config
  (setq diff-hl-update-async t))

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
                   :background nil
                   :height 120
                   :italic t)))
  :init
  (global-blamer-mode 1)
  :config
  (setq blamer-type 'margin-overlay)
  )

;;; gptel auto commit message
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
  :config
  (defun gptel-commit()
    "Generate commit message with gptel and insert it into the buffer."
    (interactive)
    (let* ((lines (magit-git-lines "diff" "--cached"))
           (changes (string-join lines "\n")))
      (gptel-request changes :system gptel-commit-prompt))))

(provide 'init-magit)
