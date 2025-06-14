;; -*- lexical-binding: t -*-
;;; meow
(use-package meow
  :straight t
  :hook (after-init . meow-global-mode)
  :demand t
  :custom-face
  (meow-normal-indicator ((t (:inherit (font-lock-function-name-face bold) :inverse-video t :height 140))))
  (meow-insert-indicator ((t (:inherit (font-lock-keyword-face bold) :inverse-video t :height 140))))
  (meow-keypad-indicator ((t (:inherit (font-lock-builtin-face bold) :inverse-video t :height 140))))
  (meow-beacon-indicator ((t (:inherit (font-lock-type-face bold) :inverse-video t :height 140))))
  (meow-motion-indicator ((t (:inherit (font-lock-doc-face bold) :inverse-video t :height 140))))
  :config
  (defun meow-mark-word-or-chinese (n)
    "Mark current word under cursor, handling both English and Chinese text.

This function uses EMT's segmentation for Chinese and default behavior for English.
The selection will be expandable with `meow-next-word' and `meow-back-word'.
The selected word will be added to `regexp-search-ring' and highlighted.

Use a negative argument to create a backward selection."
    (interactive "p")
    ;; Ensure that EMT is loaded
    (emt-ensure)
    (let* ((direction (if (< n 0) 'backward 'forward))
           (bounds (emt--get-bounds-at-point
                  (emt--move-by-word-decide-bounds-direction direction)))
           (beg (car bounds))
           (end (cdr bounds)))
      (if (eq beg end)
          ;; Use default Meow for English words
          (meow-mark-thing meow-word-thing 'word (< n 0) "\\<%s\\>")
        ;; Use EMT segmentation for Chinese
        (let* ((text (buffer-substring-no-properties beg end))
               (segments (append (emt-split text) nil))
               (pos (- (point) beg))
               (segment-bounds (car segments)))
          ;; Find the correct segment
          (dolist (bound segments)
            (when (and (>= pos (car bound)) (< pos (cdr bound)))
              (setq segment-bounds bound)))
          (when segment-bounds
            (let* ((seg-beg (+ beg (car segment-bounds)))
                   (seg-end (+ beg (cdr segment-bounds)))
                   (segment-text (buffer-substring-no-properties seg-beg seg-end))
                   (regexp (regexp-quote segment-text)))
              (let ((selection (meow--make-selection (cons 'expand 'word) seg-beg seg-end)))
                (meow--select selection (< n 0))
                (meow--push-search regexp)
                (meow--highlight-regexp-in-buffer regexp))))))))
  
  (defun meow-next-thing (thing type n &optional include-syntax)
    "Create non-expandable selection of TYPE to the end of the next Nth THING.

If N is negative, select to the beginning of the previous Nth thing instead."
    (unless (equal type (cdr (meow--selection-type)))
      (meow--cancel-selection))
    (unless include-syntax
      (setq include-syntax
            (let ((thing-include-syntax
                   (or (alist-get thing meow-next-thing-include-syntax)
                       '("" ""))))
              (if (> n 0)
                  (car thing-include-syntax)
                (cadr thing-include-syntax)))))
    (let* ((expand (equal (cons 'expand type) (meow--selection-type)))
           (_ (when expand
                (if (< n 0) (meow--direction-backward)
                  (meow--direction-forward))))
           (new-type (if expand (cons 'expand type) (cons 'select type)))
           (m (point))
           (p (save-mark-and-excursion
                (if (and (eq thing 'word) (eq system-type 'darwin))
                    (progn
                      (emt-ensure) ;; Ensure EMT is loaded
                      (if (> n 0)
                          (emt-forward-word n)
                        (emt-backward-word (- n))))
                  (forward-thing thing n))
                (unless (= (point) m)
                  (point)))))
      (when p
        (thread-first
          (meow--make-selection
           new-type
           (meow--fix-thing-selection-mark thing p m include-syntax)
           p
           expand)
          (meow--select))
        (meow--maybe-highlight-num-positions
         (cons (apply-partially #'meow--backward-thing-1 thing)
               (apply-partially #'meow--forward-thing-1 thing))))))

  (defun meow--forward-thing-1 (thing)
    (let ((pos (point)))
      (if (and (fboundp 'emt--move-by-word) (looking-at-p "\\cc"))
          (emt--move-by-word 'forward)
        (forward-thing thing 1))
      (when (not (= pos (point)))
        (meow--hack-cursor-pos (point)))))

  (defun meow--backward-thing-1 (thing)
    (let ((pos (point)))
      (if (and (fboundp 'emt--move-by-word) (looking-at-p "\\cc"))
          (emt--move-by-word 'backward)
        (forward-thing thing -1))
      (when (not (= pos (point)))
        (point))))
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
  (if (and sys/macp (equal window-system 'ns))
      (meow-normal-define-key
       '(";" . sis-meow-reverse))
    (meow-normal-define-key
     '(";" . meow-reverse)))
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
   '("w" . meow-mark-word-or-chinese)
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
