;;; meow
(use-package meow
  :straight t
  :hook (after-init . meow-global-mode)
  :demand t
  :custom-face
  (meow-normal-indicator ((t (:inherit (font-lock-function-name-face bold) :inverse-video t))))
  (meow-insert-indicator ((t (:inherit (font-lock-keyword-face bold) :inverse-video t))))
  (meow-keypad-indicator ((t (:inherit (font-lock-builtin-face bold) :inverse-video t))))
  (meow-beacon-indicator ((t (:inherit (font-lock-type-face bold) :inverse-video t))))
  (meow-motion-indicator ((t (:inherit (font-lock-doc-face bold) :inverse-video t))))
  :config
  (setq-default meow-replace-state-name-list '((normal . "N")
                                               (motion . "M")
                                               (keypad . "K")
                                               (insert . "I")
                                               (beacon . "B")))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  ;; [motion]
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  ;; [leader]
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; [normal]
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . (lambda ()
	       (interactive)
	       (if mark-active (command-execute #'electric-pair)
		   (command-execute #'meow-beginning-of-thing))))
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   ;; '("'" . repeat)
   '(":" . repeat)
   '("'" . electric-pair)
   '("(" . electric-pair)
   '("{" . electric-pair)
   '("\"" . electric-pair)
   '("<" . electric-pair)
   '("<Escape>" . ignore)
   '("C-," . ignore))
  (meow-define-keys
      'insert
    '("C-," . meow-insert-exit)))

;; Use ,, to escape from insert state to normal state
;;   (defvar meow-two-char-escape-sequence ",,")
;;   (defvar meow-two-char-escape-delay 0.5)

;;   (defun meow--two-char-exit-insert-state (s)
;;     "Exit meow insert state when pressing consecutive two keys.

;; S is string of the two-key sequence."
;;     (when (meow-insert-mode-p)
;;       (let ((modified (buffer-modified-p))
;;             (undo-list buffer-undo-list))
;; 	(insert (elt s 0))
;; 	(let* ((second-char (elt s 1))
;;                (event
;; 		(if defining-kbd-macro
;;                     (read-event nil nil)
;; 		  (read-event nil nil meow-two-char-escape-delay))))
;;           (cond
;;            ((null event) (ignore))
;;            ((and (integerp event) (char-equal event second-char))
;;             (backward-delete-char 1)
;;             (set-buffer-modified-p modified)
;;             (setq buffer-undo-list undo-list)
;;             (push 'escape unread-command-events))
;;            (t (push event unread-command-events)))))))

;;   (defun meow-two-char-exit-insert-state ()
;;     "Exit meow insert state when pressing consecutive two keys."
;;     (interactive)
;;     (meow--two-char-exit-insert-state meow-two-char-escape-sequence)
;;     (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
;; 	      #'meow-two-char-exit-insert-state))

;;; meow select same indent
(use-package meow
  :config
  (defun mag/meow-thing-same-indent-lines ()
    (let ((first-line)
          (last-line)
          (start-indent (current-indentation)))
      (save-excursion
	(setq first-line (line-number-at-pos))
	(while (and (not (bobp))
                    (zerop (forward-line -1))
                    (= (current-indentation) start-indent))
          (message "[back] line: %i " (line-number-at-pos))
          (setq first-line (line-number-at-pos))))

      (save-excursion
	(setq last-line (line-number-at-pos))
	(while (and (not (eobp))
                    (zerop (forward-line 1))
                    (= (current-indentation) start-indent))
          (message "[fwd] line: %i " (line-number-at-pos))
          (setq last-line (line-number-at-pos))))

      (message "fl: %i; ll: %i" first-line last-line)
      (cons first-line last-line)))

  (defun mag/pos-at-line (line-number beginning &optional offset)
    (let ((pos))
      (save-excursion
	(forward-line (1- (- line-number (current-line))))
	(when offset
          (forward-line offset))
	(if beginning
            (beginning-of-line)
          (end-of-line))
	(setq pos (point)))
      pos))

  (defun mag/meow-thing-same-indent (beginning-offset end-offset)
    (let ((line-nums)
          (first-line)
          (last-line))
      (setq line-nums (mag/meow-thing-same-indent-lines))
      (setq first-line (car line-nums))
      (setq last-line (cdr line-nums))
      (cons (mag/pos-at-line first-line t beginning-offset)
            (mag/pos-at-line last-line nil end-offset))))


  (defun mag/meow-thing-same-indent-inner ()
    (mag/meow-thing-same-indent 0 0))

  (defun mag/meow-thing-same-indent-bounds ()
    (mag/meow-thing-same-indent -1 1))

  (meow-thing-register 'same-indent
                       'mag/meow-thing-same-indent-inner
                       'mag/meow-thing-same-indent-bounds)

  (setq meow-char-thing-table
	'((?r . round)
          (?q . square)
          (?c . curly)
          (?a . angle)
          (?s . string)
          (?p . paragraph)
          (?l . line)
          (?f . defun)
          (?d . do/end)
          (?y . symbol)
          (?. . sentence)
          (?w . window)
          (?u . url)
          (?i . same-indent)
          (?b . buffer))))

(provide 'init-meow)
