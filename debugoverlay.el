(require 'pcase)
(require 'cl-lib)

(defgroup debugoverlay nil
  "Customizations for debugoverlay"
  :group 'debugoverlay
  :group 'faces)

(defface debugoverlay-comment
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for the debug overlay comments."
  :group 'debugoverlay)

(defcustom debugoverlay-default-regexp
  "^\\(/[^:]*\\):\\([0-9][0-9]*\\): *\\(.*\\)"
  "Default regular expression for 'debugoverlay-make-overlays'"
  :group 'debugoverlay
  :type 'regexp
  )

(defcustom debugoverlay-default-buffer-name
  "*compilation*"
  "Default buffer name 'debugoverlay-make-overlays' looks into"
  :group 'debugoverlay
  :type 'string
  )

(defvar debugoverlay-overlays nil "keep the created overlays here, so we can remove them when refreshing")

;; converts ie. list (1 2 3 4) into list ((1 . 2) (3 .4))
(defun debugoverlay--interleave-list (xs)
  (if xs
      (cons (cons (nth 0 xs) (nth 1 xs)) (debugoverlay--interleave-list (cdr (cdr xs))))))

;; Given a buffer and a regular expression, return all the matches as alist
;; each element of a list is a list (begin end (fullmatch match1 match2 ..))
(defun debugoverlay--matches-in-buffer (buffer regexp)
  (with-current-buffer buffer
    (save-match-data
      (save-excursion
	(let ((pos 0)
	      matches
	      (string (buffer-string)))
	  (while (string-match regexp string pos)
	    (push (list
		   (match-beginning 0)
		   (match-end 0)
		   (cl-loop for (begin . end) in (debugoverlay--interleave-list (match-data t))
			    collect (substring string begin end))) matches)
	    (setq pos (match-end 0)))
	  (reverse matches)
	  )))))

(defun debugoverlay-delete-overlays ()
  (interactive)
  (cl-loop for ((filename . line) . overlay) in debugoverlay-overlays
	   do (delete-overlay overlay)
	   )

  (setq debugoverlay-overlays nil)
  )

;; iterate all matching messages from *compilation* buffer and make overlays for them
(defun debugoverlay-make-overlays ()
  (interactive)
  (pcase
      (filter
       (lambda (x) (equal (buffer-name x) debugoverlay-default-buffer-name))
       (buffer-list))
    (`(,compilation)
     (let* ((matches (debugoverlay--matches-in-buffer compilation debugoverlay-default-regexp))
	    (buffers (debugoverlay--make-buffer-file-assoc))
	    (buffer-matches (cl-loop for (begin end match) in matches
				     if (get-file-buffer (nth 1 match))
				     collect (let* ((filename (nth 1 match))
						    (line-number (string-to-number (nth 2 match)))
						    (message (nth 3 match))
						    (buffer (get-file-buffer filename)))
					       (list filename line-number message buffer)
					       )))
	    (count 1))

       (debugoverlay-delete-overlays)

       (cl-loop for (filename line-number message buffer) in buffer-matches
		do (with-current-buffer buffer
		     (save-excursion
		       (goto-char (point-min))
		       (forward-line (1- line-number))
		       (let* ((bol (point))
			      (eol (progn (forward-line 1) (1- (point))))
			      (col (progn (forward-char -1) (current-column)))
			      (overlay-key (cons filename line-number))
			      (old-overlay (cdr (assoc overlay-key debugoverlay-overlays)))
			      (overlay (if old-overlay
					   old-overlay
					 (let ((new-overlay (make-overlay bol eol nil nil)))
					   (overlay-put new-overlay 'evaporate t)
					   new-overlay)))
			      (old-overlay-text (if old-overlay
						    (overlay-get old-overlay 'after-string)
						  " // "
						  ))
			      (new-overlay-text (propertize (concat old-overlay-text
								    (if old-overlay
									(concat "\n" (make-string col ?\s) " // "))
								    (format "[% 3d] %s"
									    count
									    message))
							    'face 'debugoverlay-comment))
			      )
			 (if (not old-overlay)
			     (setq debugoverlay-overlays (cons (cons overlay-key overlay) debugoverlay-overlays)))
			 (overlay-put overlay 'after-string new-overlay-text)
			 (cl-incf count)
			 )
		       )
		     )
		)
       ))))

(defun debugoverlay-insert-template ()
  (interactive)
  (insert "
#define D(VAR)                                                                                       \\
    [&]() {                                                                                          \\
        _Pragma(\"GCC diagnostic push\");                                                              \\
        _Pragma(\"GCC diagnostic ignored \\\"-Wshadow\\\"\");                                              \\
        auto _debug_var = (VAR);                                                                     \\
        std::cout << __FILE__ << \":\" << __LINE__ << \": \" << #VAR << \"==\" << _debug_var << std::endl; \\
        _Pragma(\"GCC diagnostic pop\");                                                               \\
        return _debug_var;                                                                           \\
    }()

#define E(VAR) (VAR)

#define HERE std::cout << __FILE__ << \":\" << __LINE__ << \": here\" << std::endl
"))

(defun debugoverlay--finish-functions-hook (buffer status-string)
  (debugoverlay-make-overlays)
  )

(defun debugoverlay-hook (&optional append local)
  "Arranges debugoverlay-make-overlays be called after compile finishes."
  (interactive)
  (add-hook 'compilation-finish-functions 'debugoverlay--finish-functions-hook append local)
  )

(provide 'debugoverlay)
