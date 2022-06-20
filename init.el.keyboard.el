;;; init.el.keyboard.el -- included by init.el
;;
;;; Commentary:
;;
;; Keymap Lookup Order:
;; (define-key KEYMAP KEY DEF)
;; 1. keymap overriding-terminal-local-map is for terminal-specific key binds.
;; 2. keymap overriding-local-map is for keys that override all other local keymaps.
;; 3. keymap char property at point is for keymaps that are local to the character point is at.
;; 4. keymap emulation-mode-map-alists is for advanced multi-mode keymap management.
;; 5. keymap minor-mode-overriding-map-alist is for overriding the keymaps used by minor modes in major modes.
;; 6. keymap minor-mode-map-alist is the preferred means of specifying the keymaps for minor modes.
;; 7. keymap text property at point is like the one above for char properties but is for text properties only.
;; 8. (current-local-map) is for keymaps defined in the buffers’ current local map.
;;    `(local-set-key KEY COMMAND)'
;; 9. (current-global-map) is the last place Emacs will look for key binds and it is for the global ones.
;;    (global-set-key KEY COMMAND)
;;
;; Uploaded at https://github.com/paulihano/nim-switch-buffers
;;
;;; Code:

;;;;;;;;;;;;;;;;
;; ring - a cons cell link list which assumed to form a ring
;; a ring has no beginning for end, but is referenced by a current cons
;; -- used to order buffers --
;;;;;;;;;;;;;;;;

(defun nim-ring-relative (DELTA RING)
  "Return the cons DELTA relative to the ring RING list."
  (unless (integerp DELTA)
    (error "Expecting integerp DELTA but found %s" DELTA))
  (if (or (not RING) (not DELTA))
      RING ;; done
    (while (< 0 DELTA)
      (setq RING (cdr RING))
      (setq DELTA (1- DELTA)))
    (if (eq 0 DELTA)
        RING ;; done
      (let ((zFirst RING)
            (zRelative RING))
        (while (> 0 DELTA)
          (setq RING (cdr RING))
          (setq DELTA (1+ DELTA)))
        (while (not (eq RING zFirst))
          (unless RING (error "Ring has end!"))
          (setq RING (cdr RING))
          (setq zRelative (cdr zRelative)))
        zRelative)))) ;; done

(defun nim-ring-relative-moved (DELTA RING &optional MAKE-LIST)
  "Copy RING moving current cons DELTA distance around the ring.
First member of returned ring will have the same car.
If MAKE-LIST then leave nil end."
  (unless (integerp DELTA)
    (error "Expecting integerp DELTA but found %s" DELTA))
  (when RING
    (let ((zStop (nim-ring-relative
                  (if (> 0 DELTA) (1- DELTA) DELTA) RING)))
      (let* ((zEnd (cons (car RING) nil))
             (zStart zEnd)
             (zRing (cdr zStop)))
        (while (not (eq zRing zStop))
          (unless zRing (error "Ring has end!"))
          (when (not (eq zRing RING))
            (setq zEnd (setcdr zEnd (cons (car zRing) nil))))
          (setq zRing (cdr zRing)))
        (if MAKE-LIST zStart (setcdr zEnd zStart))))))
  
(defun nim-ring-with (RING FILTER &optional MAKE-LIST)
  "Copy RING with members when FILTER(car).
If MAKE-LIST then leave nil end."
  (unless (functionp FILTER)
    (error "Expecting functionp FILTER but found %s" FILTER))
  (when RING
    (let* (zEnd
           zStart
           (zRing RING))
      (while (progn
               (unless zRing (error "Ring has end!"))
               (when (funcall FILTER (car zRing))
                 (if zEnd
                     (setq zEnd (setcdr zEnd (cons (car zRing) nil)))
                   (setq zEnd (setq zStart (cons (car zRing) nil)))))
               (not (eq (setq zRing (cdr zRing)) RING))))
      (if MAKE-LIST zStart (setcdr zEnd zStart)))))

(defvar nim-ring-next-match-default-test
  (lambda ( CAR AGAINST ) (eq CAR AGAINST))
  "Default test function for nim-ring-next-match")

(defun nim-ring-next-match (AGAINST RING &optional TEST)
  "Return existing ring whose TEST(car AGAINST) or nil if not found.
Always considers TEST(car RING) first."
  (if TEST (unless (functionp TEST)
             (error "Expecting functionp TEST but found %s" TEST))
    (setq TEST nim-ring-next-match-default-test))
  (when RING
    (catch 'return
      (let ((zRing RING))
        (while (progn
                 (unless zRing (error "Ring has end!"))
                 (when (funcall TEST (car zRing) AGAINST)
                   (throw 'return zRing))
                 (not (eq (setq zRing (cdr zRing)) RING))))) nil)))
  
;;;;;;;;;;;;;;;;
;; ringset - a ring whose car of each cons is unique
;; a ringset is a ring which inserts cars only with nim-ringset-add-car
;; -- used to order buffers --
;;;;;;;;;;;;;;;;

(defun nim-ringset-with-car (NEW-CAR RING &optional TEST)
  "When not member, create new ring with NEW-CAR inserted before RING.
Otherwise return existing member.
Use TEST to determine if NEW-CAR matches (car RING).
Default TEST to (lambda(CAR) (eq CAR NEW-CAR))."
  (let ((zNewCar NEW-CAR)) ; dynamic binding for default TEST
    (when TEST (unless (functionp TEST)
                 (error "Expecting functionp TEST but found %s" TEST)))
    (let ((zFound (nim-ring-next-match NEW-CAR RING TEST)))
      (if zFound zFound ; no copy
        (let* ((zNew (cons NEW-CAR nil))
               (zEnd zNew))
          (when RING
            (let ((zRing RING))
              (while (progn
                       (unless zRing (error "Ring has end!"))
                       (setq zEnd (setcdr zEnd (cons (car zRing) nil)))
                       (not (eq (setq zRing (cdr zRing)) RING))))))
          (setcdr zEnd zNew))))))

;;;;;;;;;;;;;;;;
;; mortal - mortal buffers are to be killed after closing its window
;; enforced by keystroke declarations
;;;;;;;;;;;;;;;;

(defun nim-buffer-is-mortal (BUFF)
  "Should BUFF be killed after closing its window?"
  (let ((zName (buffer-name BUFF)))
	 (not (or (buffer-file-name BUFF) ;; not associated with a file
			    (get-buffer-window BUFF) ;; not currently displayed in a window
			    (string-equal "*scratch*" zName) ;; not the scratch buffer
			    (string-match "^untitled(<[0-9]+>)?$" zName) ;; not a new untitled buffer awaiting save
			    (string-match "^ *Minibuf-[0-9]+*)?$" zName))))) ;; not a minibuffer

;;;;;;;;;;;;;;;;
;; buffer-type - used to determine buffers that are regularly visited vs not
;; enforced by keystroke declarations
;; -- used to limit ordered buffers --
;;;;;;;;;;;;;;;;

(defun nim-buffer-type (&optional BUFF)
  "Return the buffer type of BUFF via reference to buffer-types."
  (unless BUFF (setq BUFF (current-buffer)))
  (let ((zName (buffer-name BUFF)))
	 (cond
     ((not zName) "killed")
     ((not (buffer-live-p BUFF)) "killed")
	  ((buffer-file-name BUFF) "file")
	  ((equal "dired-mode" (with-current-buffer BUFF major-mode)) "dired")
	  ((string-equal "*scratch*" zName) "scratch")
	  ((string-equal "*Messages*" zName) "debug")
	  ((string-equal "*Backtrace*" zName)
      (if (eq buffer-saved-size 0) "empty backtrace" "debug"))
	  ((string-match "^untitled" zName) "untitled")
	  ((string-equal "*" (substring zName 0 1)) "*")
	  ((string-equal " " (substring zName 0 1)) "blank")
     (t "other"))))

(defcustom nim-buffer-types-wanted '("file" "dired" "scratch" "debug" "untitled")
  "Buffer types understood by 'nim-buffer-ringset' as being wanted.
Note: *Flycheck error messages* causes problems because it cannot become current."
  :group 'string
  :type '(set :choice '("file" :tag "represents a file"
                        "dired" :tag "acting as a directory editor"
                        "scratch" :tag "the *scratch* buffer"
                        "debug" :tag "*Messages* or *Backtrace* buffers"
                        "empty backtrace" :tag "empty *Backtrace* buffer"
                        "untitled" :tag "waiting to become associated with a file"
                        "*" :tag "standard, but internal"
                        "blank" :tag "non-standard, and internal (DONOT include this)"
                        "other" :tag "category unknown (risky)"
                        "killed" :tad "has been killed (DONOT include this)")))

(defun nim-buffer-is-wanted (&optional BUFF)
  "Is BUFF displayed in a window or of a wanted buffer type?"
  (member (nim-buffer-type BUFF) nim-buffer-types-wanted))

;;;;;;;;;;;;;;;;
;; buffer-ringset - used to determine apparent buffer order
;; enforced by keystroke declarations
;; -- used to order buffers --
;;;;;;;;;;;;;;;;

(defvar _nim-buffer-ringset '()
  "The last known ring of wanted buffers.
Use 'nim-buffer-ringset' for a updated buffer ringset.")
(setq _nim-buffer-ringset nil) ;; rebuild malformed ringset

(defun nim-buffer-ringset (&optional BUFF)
  "Verify wanted buffers in ordered ringset.
The car value of the result will be BUFF or 'current-buffer'.
Filters unwanted buffers and inserts wanted buffers.
Return value is stored in '_nim-buffer-ringset'."
  (let ((zRing (nim-ring-with _nim-buffer-ringset #'nim-buffer-is-wanted)))
    ;; insert wanted
    (dolist (zBuff (buffer-list))
      (when (nim-buffer-is-wanted zBuff)
        (setq zRing (nim-ringset-with-car zBuff zRing))))
    ;; assign current-buffer
    (if BUFF (unless (bufferp BUFF)
               (error "Expecting bufferp BUFF but found %s" BUFF))
      (setq BUFF (current-buffer)))
    (setq _nim-buffer-ringset (nim-ringset-with-car BUFF zRing))))

(defun nim-buffer (&optional DELTA BUFF)
  "Return the buffer DELTA relative to BUFF.
Use this to determine new buffer during [C-next] and [C-prior].
Stores cons holding result in '_nim-buffer-ringset'."
  (car (setq _nim-buffer-ringset (nim-ring-relative DELTA (nim-buffer-ringset BUFF)))))

(defun nim-buffer-move (DELTA &optional BUFF)
  "Shift BUFF in 'nim-buffer-ringset' DELTA relative to its current position.
Used to rearrange buffer order during [C-S-next] and [C-S-prior]."
  (setq _nim-buffer-ringset
        (nim-ring-relative-moved DELTA (nim-buffer-ringset BUFF))))

;;;;;;;;;;;;;;;;
;; singleton routines used by keystroke declarations
;;;;;;;;;;;;;;;;

(defun nim-join-line()
  "Join the next line onto the end of the current line and goto the junction."
  (interactive)
  (end-of-line)
  (let ((zPoint (point)))
	 (next-line)
	 (delete-indentation)
	 (goto-char zPoint)))

(defun nim-join-paragraph ()
  "Join all lines not separated by a blank line.
Should unjoin a single line separated by blank lines."
  (interactive)
  (let ((zFirstIsBlank (= (line-beginning-position) (line-end-position))))
	 (next-line)
	 (while (not (= (line-beginning-position) (line-end-position)))
		(if zFirstIsBlank
			 (setq zFirstIsBlank nil)
		  (delete-indentation))
		(next-line))))

(defun nim-kill-whole-lines ()
  "Kill whole lines."
  (interactive)
  (if (not (use-region-p))
      (kill-whole-line)
	 (when (> (point) (mark)) (exchange-point-and-mark))
	 (beginning-of-line) ; enclude entire line at start of region
	 (exchange-point-and-mark)
	 (unless (bolp)
		(forward-line)
		(beginning-of-line)) ; do not extend region if end of region is at bol
	 (kill-region (mark) (point))))

(defun nim-kill-mortal ()
  "Kill all mortal buffers."
  (interactive)
  (let ((zBuff (current-buffer))
        zKilled
        zName)
	 (dolist (zOther (buffer-list))
		(and (not (eq zBuff zOther))
			  ;; keep the buffer-name to create a killed list for the message
			  (setq zName (buffer-name zOther))
			  (nim-buffer-is-mortal zOther)
			  (kill-buffer zOther)
			  ;; zOther successfully killed
			  (setq zKilled (push zName zKilled))))
	 (if (car zKilled)
		  (message "Killed %d mortal non-file buffers %s."
                 (length zKilled) (format "%s " zKilled))
		(message "No mortal non-file buffers remain."))))

(defun nim-kill-window ()
  "Close window properly.
If it is the last window of last frame then 'kill-buffer'.
Otherwise, delete window/frame killing buffer only when 'buffer-is-mortal'.
Unless *scratch*, then 'nim-kill-mortal' (leave scratch open and garbage collect)."
  (interactive)
  (if (string-equal "*scratch*" (buffer-name))
      (nim-kill-mortal)
    (let ((zBuff (current-buffer)))
		(if (not (or (cdr (window-list nil "no-minibuf"))
					    (cdr (frame-list))))
			 (kill-buffer) ;; is last window of last frame
		  (if (and
				 (cdr (frame-list))
				 (not (cdr (window-list nil "no-minibuf"))))
				(delete-frame) ;; is last winodw with more frames
			 (delete-window)) ;; more windows in the frame
		  (when (nim-buffer-is-mortal zBuff) (kill-buffer zBuff))))))

(defun nim-one-window ()
  "Cause 'selected-window' to be the only window and tidy other buffers."
  (interactive)
  (let ((zWind (selected-window)))
	 (dolist (zOther (window-list))
		(when (not (eq zOther zWind))
		  (let ((zBuff (window-buffer zOther)))
			 (delete-window zOther)
			 (when (nim-buffer-is-mortal zBuff)
				(kill-buffer zBuff)))))))

(defun nim-toggle-binary ()
  "Rotate between mixed-octal (normal), mixed-hex, and hexl."
  (interactive)
  (cond
	((and (not (eq 'hexl-mode major-mode))
         (not buffer-display-table))
	 (hexl-mode)
    (message "(hexl-mode)"))
	((eq 'hexl-mode major-mode)
    (hexl-mode-exit)
    (defvar mixed-hex-display-table
      (progn
        (let ((zTable (make-display-table)))
	       (cl-loop
	        for x in
           (append (number-sequence 127 255)
                   (number-sequence 0 8)
                   (number-sequence 11 31))
	        do (aset zTable (unibyte-char-to-multibyte x)
				        (cl-map 'vector
							       (lambda (CHAR) (make-glyph-code CHAR 'escape-glyph))
							       (format "\\%02x" x))))
          zTable))
      "A display-table showing unusual ASCII characters as hex values.")
	 (setq buffer-display-table mixed-hex-display-table)
    (message "(setq buffer-display-table mixed-hex-display-table)"))
	(t
	 (setq buffer-display-table nil)
    (message "(setq buffer-display-table nil) ; standard-display-table uses octal"))))

(defun nim-toggle-case ()
	"Rotate CAPS, Capitalized, lower."
	(interactive)
	(let (zStart
         zEnd
         zOriginal
         zState
         (deactivate-mark nil))
		(if (use-region-p)
				(setq zStart (region-beginning) zEnd (region-end))
			(save-excursion
			  (skip-chars-backward "[:alnum:]-_")
				(setq zStart (point))
				(skip-chars-forward "[:alnum:]-_")
				(setq zEnd (point))))
		(when (not (eq last-command this-command))
		  (put this-command 'state 0))
      (setq zOriginal (buffer-substring-no-properties zStart zEnd))
      (setq zState (get this-command 'state))
      (defun nim-toggle-case--do-state-0 ()
        (upcase-region zStart zEnd)
	     (put this-command 'state 1)
        (when (string-equal zOriginal (buffer-substring-no-properties zStart zEnd))
          (nim-toggle-case--do-state-1))
        (when (equal 0 (get this-command 'state))
          (message "No alphabetic present for case change.")))
      (defun nim-toggle-case--do-state-1 ()
        (downcase-region zStart zEnd)
	     (upcase-initials-region zStart zEnd)
	     (put 'nim-toggle-case 'state 2)
        (when (string-equal zOriginal (buffer-substring-no-properties zStart zEnd))
          (nim-toggle-case--do-state-2)))
      (defun nim-toggle-case--do-state-2 ()
        (downcase-region zStart zEnd)
	     (put this-command 'state 0))
		(cond ((equal 0 zState) (nim-toggle-case--do-state-0))
		      ((equal 1 zState) (nim-toggle-case--do-state-1))
				((equal 2 zState) (nim-toggle-case--do-state-2)))))

(defun nim-toggle-desktop ()
  "Toggle between .emacs.desktop in $PWD and $HOME/ or $HOME/.emacs.d/."
  (interactive)
  (let ((the-dir (file-name-as-directory desktop-dirname))
		  (the-pwd (file-name-as-directory (expand-file-name (getenv "PWD"))))
		  (the-home (file-name-as-directory (expand-file-name (getenv "HOME")))))
	 (if (equal the-dir the-home)
		  (if (equal the-pwd the-home)
				(desktop-change-dir (concat the-home ".emacs.d/"))
			 (desktop-change-dir the-pwd))
		(desktop-change-dir the-home))))

(defun nim-toggle-readonly ()
  "Toggle read-only mode using find-alternative-file-with-sudo if not file-writable."
  (interactive)
  (if (and
		 (buffer-file-name)
		 (not (file-writable-p (buffer-file-name))))
		(find-alternative-file-with-sudo) ;; then ;; root privileges required
    (read-only-mode (if buffer-read-only -1 1))))

(defun nim-toggle-narrow ()
  "Narrowing toggle.
Create and remove indirect buffers so that this narrowing is not shared with other buffers."
  (interactive)
  (if (buffer-narrowed-p)
		;; then widen and kill the buffer if it is indirect
		(let ((zBuff (current-buffer)) (the-base (buffer-base-buffer)))
		  (if (not the-base) (widen)
			 (set-window-buffer nil the-base)
			 (unless (get-buffer-window zBuff)
				(kill-buffer zBuff)))) ;; C-x n w
	 ;; else clone and narrow the buffer
	 (switch-to-buffer (clone-indirect-buffer nil nil))
	 (if (not (use-region-p))
		  (narrow-to-defun) ;; C-x n d
		(let ((the-begin (region-beginning)) (zEnd (region-end)))
		  (transient-mark-mode 0) ;; kill active region hightlighting while possible
		  (narrow-to-region the-begin zEnd)))))
  
;;;;;;;;;;;;;;;;
;; get unwanted key declarations out of the way
;;;;;;;;;;;;;;;;

(defun nim-remove-key (keymap key)
	"Fought hard to rid myself of [C-return] and cua-C-x nasty binding.
Region deletes defined here always use the paste buffer, therefore C-x has to editing purpose.
Probably need this to master Ctrl-[ which loves to come back with ESC."
	;; pulled this off of the web and it is beyond my understanding
   (define-key keymap key nil)
   (setq key (cl-mapcan (lambda (k)
                          (if (and (integerp k)
                                   (/= (logand k ?\M-\^@) 0))
                              (list ?\e (- k ?\M-\^@))
                            (list k)))
                        key))
   (if (= (length key) 1)
       (delete key keymap)
     (let* ((prefix (vconcat (butlast key)))
            (submap (lookup-key keymap prefix)))
       (delete (last key) submap)
       (when (= (length submap) 1)
         (nim-remove-key keymap prefix)))))

(nim-remove-key cua-global-keymap (kbd "<C-return>"))
(nim-remove-key cua--prefix-repeat-keymap (kbd "C-x"))

;;;;;;;;;;;;;;;;
;; declare minor mode to give precedence to key declarations
;;;;;;;;;;;;;;;;

;; Main use is to have my key bindings have the highest priority
(defvar nim-mode-map (make-sparse-keymap) "")

(define-minor-mode nim-mode
  "A minor mode to override major mode keybindings."
  :init-value t ;; needed to get enabled in `fundamental-mode' buffers even after \"(global-nim-mode 1)\".
  :lighter " nim"
  :keymap nim-mode-map)
(provide 'nim-mode) ;; add `nim-mode' to global variable `features'
(define-globalized-minor-mode global-nim-mode nim-mode (lambda()(nim-mode t))) ;; turn on in every buffer
;; `emulation-mode-map-alists' maps preside over `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((nim-mode . ,nim-mode-map)))

;; Turn off the minor mode in the minibuffer
;;(add-hook 'minibuffer-setup-hook nim-mode)

;; without [tab] declaration, tab will be converted to C-i
;;
;; "[C-.]" [(control .)] -- I have success with this syntax
;; "[^.]"  [(control .)]
;; "[A-.]" [(alt .)] -- No longer on keyboards.
;; "[S-.]" [(shift .)]
;; "[s-.]" [(super .)] -- I reserve this for the window manager.
;; "[H-.]" [(hyper .)] -- No longer of keyboards.
;; "[M-.]" [(meta .)] -- Meta is called Alt on keyboard
;; "[m-#]" mouse key number -- The mouse is on the floor again.
;;
;; "[C-S-.]" specifies both control and shift plus the lower case letter
;; "[M-.]" specifies meta and the case of the letter determines the inclusion of shift
;; Note: Mixing the above two syntaxes in any way does not work.
;;
;; kbd
;;   <C-down> = C-<down> = ^<down>
;;
;; C-[ is ESC as defined by terminals -- makes a mess if you redefine it even in X

;; prevent need to backspace twice when trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(bind-keys :map nim-mode-map ;; * evidently overrides all minor modes
			  ([escape] . (lambda ()
								 "Needs to overload all minibuffer keyboard maps."
								 (interactive)
								 (if (window-minibuffer-p)
									  (minibuffer-keyboard-quit) ;; certainly not ideal -- minibuffer*map uses 27 (not escape)
									(keyboard-quit))))
			  ([delete] . (lambda ()
								 "Kill active region to the yank stack.
Otherwise, perform nomral delete.
Use backspace for an emacs delete into register 0."
								 (interactive)
								 (if (use-region-p)
									  (delete-active-region t) ;; yank the region onto the stack
									(delete-char 1)))) ;; forget singly removed characters
			  ([M-up] . (lambda ()
							  "Scroll-other-window up if one exists, otherwise scroll-up this window."
							  (interactive)
							  (if (< 1 (count-windows)) (scroll-other-window -1) (scroll-up -1))))
			  ([M-down] . (lambda ()
								 "Scroll-other-window down if one exists, otherwise scroll down this window."
								 (interactive)
								 (if (< 1 (count-windows)) (scroll-other-window 1) (scroll-up 1))))
			  ;; foward|backward-sexpression does NOT work properly if beginning inside quotes.
			  ;;   need to be able to move outside of a quoted string.
			  ;; foward|backward-sexpression will error only if immediately before|after end|beginning of sexpression.
			  ;;   the error can be used to flag when we are actually there.
			  ;; @@@ Repair examining the font for inside quotes and move until the font changes. I can't remember the name of the commands, but those exist. @@@
			  ([M-left] . (lambda ()
								 "Previous sexpression."
								 (interactive "^")
								 (condition-case nil (backward-sexp)
									(error (backward-char)))))
			  ([M-S-left] . nil) ;; handled by M-left via (interactive "^")
			  ([M-right] . (lambda ()
								  "Next sexpression."
								  (interactive "^")
								  (condition-case nil (forward-sexp)
									 (error (forward-char)
											  (condition-case nil
													(foward-sexp) (error nil))))))
			  ([M-S-right] . nil) ;; handled by M-right via (interactive "^")
			  ;; this concept of saving and restoring the point is messy and unnecessary.
			  ;; a package is installed which records the point logically and all I need is a pop-point (whatever that is).
			  ("C-SPC" . point-to-register) ; store point vs C-x r SPC a-z
			  ("C-S-SPC" . jump-to-register) ; recall point vs C-x r j a-z
			  ("C-=" . text-scale-increase) ; vs C-x + modal (lazy on the shift key like others)
			  ("C-+" . describe-text-properties) ; vs describe-face C-u C-x =
			  ("C--" . text-scale-decrease) ; vs C-x - modal
			  ;; ("M-=" . count-words-region) ; vs M-=
			  ;; the multi-cursor mode stuff is still under review
			  ("C->" . mc/mark-next-like-this) ;; when text is selected
			  ("C-<" . 'mc/mark-previous-like-this) ;; when text is selected
			  ("C-]" . nim-toggle-narrow) ;; wish this was C-[ because it is easier to type and makes a good visual. ;; C-x n n
			  ;; ([return] . newline) ;; needed to distinguish from C-m  ;;;; overloads way no much
			  ([C-return] . (lambda () ;; was cua-set-rectangle-mark
									"If 'use-region-p' then multi-cursor stuff is still in review.
Otherwise cua-set-rectangle-mark is useful."
									(interactive)
									(if (not (use-region-p))
										 (cua-set-rectangle-mark) ;; when no selection
									  (if (= 1 (count-lines (region-beginning) (region-end)))
											(mc/mark-all-like-this) ;; when simple selection
										 (mc/edit-lines))))) ;; when muli-line selection
           ([M-return] . flyspell-correct-word-before-point)
			  ([C-next] . (lambda ()
                         "Like 'next-buffer', but remember buffer order."
                         (interactive)
                          (switch-to-buffer (nim-buffer 1)))) ; vs scroll-left
			  ([C-prior] . (lambda ()
                          "Like 'previous-buffer', but remember buffer order."
                          (interactive)
                          (switch-to-buffer (nim-buffer -1)))) ; vs scroll-right
			  ([C-S-next] . (lambda ()
                           "Move buffer foward 1 position in list used by [C-next] and [C-prior]."
                           (interactive)
                           (nim-buffer-move 1))) ; vs scroll-left ([C-S-prior] . (lambda ()
			  ([C-S-prior] . (lambda ()
                            "Move buffer backward 1 position in list used by [C-next] and [C-prior]."
                            (interactive)
                            (nim-buffer-move -1))) ; vs scroll-right
			  ("C-1" . nim-one-window ) ; vs C-x 1 ; was digit-argument
			  ("C-2" . split-window-below) ; vs C-x 2 ; was digit-argument
			  ("C-3" . split-window-right) ;  vs C-x 3 ;  was digit-argument
			  ("C-6" . enlarge-window) ; vs C-x C-^ ; was digit-argument
			  ("C-^" . shrink-window) ; was digit-argument
			  ("C-7" . enlarge-window-horizontally) ; vs C-x {
			  ("C-&" . shrink-window-horizontally) ; vs C-x }
			  ("C-a" . (lambda (iPoint)
							 "Goto logical start of line, physical start of line, or select all when empty line."
							 (interactive "^d")
                      (back-to-indentation)
							 (when (eq iPoint (point))
                        (if (bolp)
                            (if (not (use-region-p))
                                (mark-whole-buffer)
                              (pop-mark)
                              (goto-char (mark))))
                        (beginning-of-visual-line)))) ; vs beginning-of-visual-line
			  ("C-b" . list-buffers ) ; vs C-x C-b
			  ("C-S-b" . nim-toggle-binary)
			  ("M-c" . kill-region); kill vs C-w
			  ("M-d" . nim-toggle-desktop) ; vs delete-char
			  ;; ("C-e") ; end-of-visual-line
			  ;; ("C-S-e") ; extend selection to end-of-visual-line
			  ("M-e" . eval-last-sexp) ; vs C-x C-e
			  ("M-E" . (lambda ()
                      "Evaluate buffer with message"
                      (interactive)
                      (message "eval-buffer %s" (current-buffer))
                      (eval-buffer))) ; was forward-sentence
			  ("C-f" . isearch-forward-regexp)
			  ("C-S-f" . (lambda ()
							   "Toggle iedit using available region."
							   (interactive)
							   (condition-case the-error (iedit-mode)
								  (error (message "Could not find an i-edit word against the current point." )))))
			  ("C-g" . (lambda ()
							 "Next iedit or isearch match allowing used region to replace isearch-string."
							 ;; Note: Selected region is not the same as found region is not the same as...
							 (interactive)
							 (if (use-region-p)
								  (progn
									 (setq isearch-string
											 (regexp-quote
											  (buffer-substring-no-properties (region-beginning) (region-end))))
									 (deactivate-mark)
									 (isearch-repeat-forward))
								(if iedit-mode
									 (iedit-next-occurrence 1)
								  (isearch-repeat-forward))))) ; search vs C-M-s
			  ("C-S-g" . (lambda ()
							   "Previous iedit or isearch match allowing used region to replace isearch-string."
							   ;; Note: Selected region is not the same as found region is not the same as...
							   (interactive)
							   (if (use-region-p)
								    (progn
									   (setq isearch-string
											   (regexp-quote
											    (buffer-substring-no-properties (region-beginning) (region-end))))
									   (deactivate-mark)
									   (isearch-repeat-backward))
								  (if iedit-mode
									   (iedit-prev-occurrence 1)
								    (isearch-repeat-backward)))))
			  ("C-S-h" .
				(lambda () (interactive)
				  (hs-minor-mode 1)
				  (if (hs-find-block-beginning)
						(hs-toggle-hiding)
					 (let ((from (point-min))
							 (to (point-max)))
						(while (and
								  (not (hs-overlay-at from))
								  (setq from (next-overlay-change from))
								  (not (= to from)))) ; locate first hs-overlay
						(if (= to from) ; hs-overlay-was-not-found
							 (hs-hide-all)
						  (hs-show-all))))))
			  ([tab] . ; without <tab> declaration, tab will be converted to C-i
				(lambda () (interactive)
				  (cond ((minibufferp) (minibuffer-complete))
						  (buffer-read-only (forward-button 1 t t))
						  (t (indent-for-tab-command)))))
			  ([S-tab] . ; without <tab> declaration, tab will be converted to C-i
				(lambda () (interactive)
				  (cond (buffer-read-only (forward-button -1 t t))
						  (t (indent-according-to-mode)))))
			  ("C-i" . (lambda () (interactive)
							 "Insert a line before current line."
							 (forward-line '-1) (end-of-line) (newline-and-indent))) ; vs kill-visual-line (until eol)
			  ("C-S-i" . (lambda () (interactive)
								"Insert a line after current line."
								(end-of-line) (newline-and-indent)))
           ("M-i" . edebug-defun)
			  ("C-j" . nim-join-line)
			  ("C-S-j" . nim-join-paragraph)
			  ("C-k" . nim-kill-whole-lines) ; vs kill-visual-line (until eol)
			  ;; C-l . recenter-top-bottom
			  ("C-S-l" . (lambda ()
								"Scroll the window to horizontally center the point."
								(interactive)
								(let ((mid (/ (window-width) 2))
										(line-len (save-excursion (end-of-line) (current-column)))
										(cur (current-column)))
								  (if (< mid cur)
										(set-window-hscroll (selected-window)
																  (- cur mid))))))
			  ;; "c-m" must overload [return] to unbind for this overload
			  ("C-S-m" . (lambda ()
								"Toggle buffer font between Monospace and normal defaut.
Assumes buffer-face-mode is indicative of this which is obviously stupid."
								(interactive)
								(if (and (boundp 'buffer-face-mode) buffer-face-mode)
									 (buffer-face-mode 0)
								  (buffer-face-set :family "Monospace"))))
			  ("C-n" . flycheck-next-error) ; vs next-line ; this needs emacs version 26
			  ("C-S-n" . (lambda ()
								"Create a new empty buffer."
								(interactive)
								(switch-to-buffer (generate-new-buffer (generate-new-buffer-name "untitled")))))
			  ("C-o" . other-window) ; other vs C-x o
			  ("C-S-o" . find-file) ; vs ?
			  ("C-p" . flycheck-previous-error)
			  ("C-S-p" . (lambda ()
								"Preview a latex buffer or selected region."
								(interactive)
								(if (use-region-p)
									 (tex-region (region-beginning) (region-end))
								  (tex-buffer))
								(run-with-idle-timer 0.3 nil 'tex-view))) ; vs foward-line
           ("C-r" . nim-toggle-readonly) ; replacing isearch-backward
			  ("C-S-r" . insert-file)
           ("M-r" . find-alternate-file) ; vs C-x C-v ; more useful than revert-buffer
           ("M-R" . auto-revert-mode)
			  ("C-s" . (lambda () (interactive) (basic-save-buffer))) ; vs C-x C-s
			  ("C-S-s" . (lambda ()
								"Append entire buffer or selected region to another file."
								(interactive)
								(let ((zFileName
                               (read-file-name  ; FILENAME ; into this file name
										  (concat "Write " (if (use-region-p) "selected region" "buffer") " to:"))))
								  (when zFileName
									 (progn
										(if (use-region-p)
											 (write-region (region-beginning) (region-end) zFileName) ; append region to file
										  (write-file zFileName))
										(find-file zFileName)))))) ; open the file
			  ("C-t" . toggle-truncate-lines)
			  ("C-S-t" . nim-toggle-case) ; vs transpose-chars
			  ("C-S-v" . cua-yank-pop) ; vs cua-yank ; better if cua-paste-pop-rotate-temporarily
			  ("C-w" . nim-kill-window) ; vs kill-region
			  ("C-v" . cua-paste)
			  ("C-x C-f" . find-file-at-point) ; vs find-file
			  ;; "M-x" . execute-extended-command
			  ("M-S-x" .  eval-expression) ;; was M-: or M-S-; ;;;; I have no idea why this does not work
			  ("C-y" . (lambda ()
							 "Insert the paste buffer at beginning of line for insertion whole lines."
							 (interactive)
							 (beginning-of-line)
							 (cua-paste nil)))
			  ("C-S-y" . cua-paste-pop) ; vs M-Y
			  ("C-z" . undo-fu-only-undo) ; undo vs C-_
			  ("C-S-z" . undo-fu-only-redo) ; redo vs C-g C-_ where C-g is a toggle ;  may (keyboard-quit) [control-g] to redo after logical top
			  )
