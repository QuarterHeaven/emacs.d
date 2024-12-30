;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-obsolete-function-alias 'after-load 'with-eval-after-load "")

;;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

;;; String utilities missing from core emacs

(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;; insert image from clipboard; from https://emacs-china.org/t/topic/6601/3
(defun org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (if *is-a-mac*
      (progn
       (let* ((path (concat default-directory "img/"))
	      (image-file (concat
			   path
			   (buffer-name)
			   (format-time-string "_%Y%m%d_%H%M%S.png"))))
	 (if (not (file-exists-p path))
	     (mkdir path))
	 (do-applescript (concat
			  "set the_path to \"" image-file "\" \n"
			  "set png_data to the clipboard as «class PNGf» \n"
			  "set the_file to open for access (POSIX file the_path as string) with write permission \n"
			  "write png_data to the_file \n"
			  "close access the_file"))
	 ;; (shell-command (concat "pngpaste " image-file))
	 (org-insert-link nil
			  (concat "file:" image-file)
			  "")
	 (message image-file))
       (org-display-inline-images)
       )
    (progn
     (let* ((path (concat default-directory "img/"))
	   (image-file (concat
			path
			(buffer-name)
			(format-time-string "_%Y%m%d_%H%M%S.png"))))
      (if (not (file-exists-p path))
	  (mkdir path))
      (shell-command (concat "pngpaste " image-file))
      (org-insert-link nil (concat "file:" image-file) ""))
    ;; (org-display-inline-images) ;; inline显示图片
     )))

;;; insert image under wsl2, from https://emacs-china.org/t/wsl-org/14100/2
(defun my-yank-image-from-win-clipboard-through-powershell()
  "to simplify the logic, use c:/Users/Public as temporary directoy, then move it into current directoy

Anyway, if need to modify the file name, please DONT delete or modify file extension \".png\",
otherwise this function don't work and don't know the reason
"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name (format "%s" (read-from-minibuffer "Img Name:" (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))))
         ;; (file-path-powershell (concat "c:/Users/\$env:USERNAME/" file-name))
         (file-path-wsl (concat "./img/" file-name)))
    (if (file-exists-p "./img")
        (ignore)
      (make-directory "./img"))
    ;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
    (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
    (rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)
    (format "%s" file-path-wsl)
    ))

(defun org-insert-image-wsl ()
  "call `my-yank-image-from-win-clipboard-through-powershell' and insert image file link with org-mode format"
  (interactive)
  (let* ((file-path (my-yank-image-from-win-clipboard-through-powershell))
         (file-link (format "file:%s" file-path (file-name-sans-extension (file-name-nondirectory file-path))))
         )
    (org-insert-link nil file-link "")
    ))

;;; keymap for resize window
(defvar resize-window-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "^" 'enlarge-window)
      (define-key map "}" 'enlarge-window-horizontally)
      (define-key map "{" 'shrink-window-horizontally)
      (define-key map "v" 'shrink-window)
      map)
    "Keymap to repeat window resizing commands.  Used in `repeat-mode'.")
(put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
(put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window 'repeat-map 'resize-window-repeat-map)

;;; fonts set copy from centaur
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("TriplicateT4c Nerd Font" "BlexMono Nerd Font Mono" "Jetbrains Mono"
                           "SF Mono" "SF Pro Display"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 140)
                                                      (sys/win32p 110)
						      (sys/WSL 200)
                                                      (t 110))))

    ;; Set mode-line font
    (cl-loop for font in '("TriplicateT4c Nerd Font" "BlexMono Nerd Font Mono" "Unifont Upper" "Noto Color Emoji" "SF Pro Display" "Helvetica")
             when (font-installed-p font)
             return (progn
                      (set-face-attribute 'mode-line nil :family font :height (cond (sys/macp 140)
										    (sys/win32p 110)
										    (sys/WSL 200)
										    (t 110)))
                      (when (facep 'mode-line-active)
                        (set-face-attribute 'mode-line-active nil :family font :height (cond (sys/macp 140)
											     (sys/win32p 110)
											     (sys/WSL 200)
											     (t 110))))
                      (set-face-attribute 'mode-line-inactive nil :family font :height (cond (sys/macp 140)
											     (sys/win32p 110)
											     (sys/WSL 200)
											     (t 110)))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Unifont Upper" "Segoe UI Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)

		      (set-fontset-font t 'symbol (font-spec :family font
							      :height (cond (sys/macp 140)
									    (sys/win32p 110)
									    (sys/WSL 200)
									    (t 110))))))
		      ;; (set-fontset-font t 'unicode (font-spec :family font
		      ;; 					      :height (cond (sys/macp 130)
    ;; 							    (sys/win32p 110)
		      ;; 							    (t 100))) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font
							      :height (cond (sys/macp 140)
									    (sys/win32p 110)
									    (sys/WSL 200)
									    (t 110))) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font
							    :height (cond (sys/macp 140)
									  (sys/win32p 110)
									  (sys/WSL 200)
									  (t 110)))))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("FZLiuGongQuanKaiShuS" "TsangerJinKai02" "LXGW WenKai Mono" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.0)))
                      (set-fontset-font t '(#x4e00 . #x9fa5) (font-spec :family font
									:height (cond (sys/macp 140)
										      (sys/win32p 110)
										      (sys/WSL 200)
										      (t 110))))
		      (set-fontset-font t '(#xff00 . #xffef) (font-spec :family font
									:height (cond (sys/macp 140)
										      (sys/win32p 110)
										      (sys/WSL 200)
										      (t 110))))
		      (set-fontset-font t '(#x3000 . #x303f) (font-spec :family font
									:height (cond (sys/macp 140)
										      (sys/win32p 110)
										      (sys/WSL 200)
										      (t 110))))))
    (set-fontset-font t '(880 . 1023) (font-spec :family "azukifontL"))
    (setq-default face-font-rescale-alist '(("FZLiuGongQuanKaiShuS" . 1.2)))
    (setq-default face-font-rescale-alist '(("Victor Mono" . 1.0)))
    ))

;;; lsp-bridge set path
(defun lsp-bridge-set-project-path ()
	(interactive)
	(setq lsp-bridge-get-project-path-by-filepath
		  (lambda (filepath)
			(save-match-data
			  (and (string-match default-directory filepath)
				   (match-string 0 filepath))))))

;;; Define an advice called SYMBOL and add it to PLACES.
(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;;; emacs-q test
(defun z/emacs-Q-test ()
  "Run emacs -Q async for packages you choose."
  (interactive)
  (let* ((pkgs    (completing-read-multiple "Packages: " features))
         (process (start-process
                   "*emacs-Q*" "*emacs-Q*"
                   (concat invocation-directory invocation-name)
                   "-Q"

                   ;; EVAL basics before everything
                   "--eval" "(progn                           \
(setq debug-on-error t)                                       \
(setq load-prefer-newer t)                                    \
)"
                   ;; LOAD PATH from current running emacs
                   "--eval" (format "(setq load-path '%s)"
                                    (with-output-to-string (prin1 load-path)))

                   ;; LOAD some goodies first
                   "--eval" "(progn                           \
                                                              \
(defun sk-stop-using-minibuffer ()                            \
  (when (and (>= (recursion-depth) 1)                         \
             (active-minibuffer-window))                      \
    (top-level)))                                             \
(add-hook 'mouse-leave-buffer-hook 'sk-stop-using-minibuffer) \
                                                              \
(require 'vertico)                                            \
(vertico-mode 1)                                              \
                                                              \
(require 'orderless)                                          \
(setq completion-styles '(orderless basic emacs22))           \
                                                              \
)"

                   ;; LOAD testing packages
                   "--eval" (format "(dolist (pkg '%s) (require (intern-soft pkg)))" pkgs)

                   ;; EVAL: more
                   "--eval" "(progn                           \

)")))
    (set-process-sentinel
     process
     (lambda (proc _)
       (kill-buffer (process-buffer proc))))))


(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))


;;; org mode insert file as base64
;; from https://mbork.pl/2017-12-04_Embedding_files_in_Org-mode
(defun org-insert-file-as-base64 (filename)
  "Insert Elisp code block recreating file named FILENAME."
  (interactive "f")
  (let ((base64-string
	 (with-temp-buffer
	   (insert-file-contents-literally filename)
	   (base64-encode-region (point-min) (point-max))
	   (buffer-string))))
    (insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))

;;; download file from url
(defun download-file-from-internet (file url)
  "Get a `FILE' from the internet at some `URL'."
  (when (not (file-exists-p file))
    (make-directory (file-name-directory file) t)
    (url-copy-file url file)
    (message "Dependency %s was installed from %s" file url)))

;;; tab for company and copilot
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))


(provide 'init-utils)
