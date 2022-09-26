;; TODO: add parsing for repeaters

(require 'ts)

(defun company-date--add-number-suffix (num)
  "create the suffix for a number"
  (pcase (if (numberp num) 
	     (number-to-string num)
	   num)
    ((pred (s-ends-with-p "11")) "th")
    ((pred (s-ends-with-p "12")) "th")
    ((pred (s-ends-with-p "13")) "th")
    ((pred (s-ends-with-p "1")) "st")
    ((pred (s-ends-with-p "2")) "nd")
    ((pred (s-ends-with-p "3")) "rd")
    (_ "th")))

(defun company-date--create-other-time-candidates (string)
  "add extra date completions. Accept an org active timestamp, return a list
of extra candidates."
  (let ((timep (company-date--string-contains-time-p string))
	(ts (ts-parse string)))
    (cond (timep (list (ts-format
			(concat "%A the %d"
				(company-date--add-number-suffix (ts-day ts))
				" at %H:%M")
			ts)
		       (downcase (ts-format "%m/%d/%Y at %I:%M%p" ts))
		       (downcase (ts-format (concat "%A, %B %d"
						    (company-date--add-number-suffix (ts-day ts))
						    " at %I:%M%p")
					    ts))))
	  
	  (t (list (ts-format (concat "%A the %d"
				      (company-date--add-number-suffix (ts-day ts)))
			      ts)
		   (ts-format "%m/%d/%Y" ts)
		   (ts-format (concat "%A, %B %d"
				      (company-date--add-number-suffix (ts-day ts)))
			      ts))))))



(defcustom company-date-prefix "<<"
  "Prefix to invoke date completion.")

		   (defcustom company-date-termination-suffix ">"
		     "Termination suffix. If this string is found after `company-date-prefix'
then company will not attempt to complete.")

		   (defvar company-date--past-dates nil
		     "past five dates that were completed")

		   (defconst company-date-re
		     (rx 
		      (eval company-date-prefix)
		      (+? (not ">")))
		     "RE to trigger a completion.")

		   (defcustom company-date-put-date-in-headline? 'inactive
		     "active, inactive, or t or nil")

		   (defun company-date-add-timestamp (arg)
		     "adsf"
		     (save-excursion 
		       (company-date--push-to-history arg)
		       (pcase (list (org-at-heading-p)
				    (s-starts-with-p "<" arg)
				    company-date-put-date-in-headline?)
			 ((or `(t t ,(or 'inactive 'nil))
			      `(t nil ,(or 'active 'nil)))
			  (delete-char (* -1 (length arg)))
			  (company-date--goto-body-start)
			  (insert arg "\n")))))

		   (defun company-date--goto-body-start ()
		     "Go to the start of the body of the current node,
and return the point. The start of the body is defined as
the point after the planning line, drawers immediately following
the planning line, and any closing note."
		     (org-end-of-meta-data t)
		     (let ((section 
			    (cddar
			     (org-element--parse-elements (save-excursion (org-back-to-heading)
									  (org-end-of-meta-data t)
									  (point))
							  (or (save-excursion (outline-next-heading))
							      (point-max))
							  'first-section nil nil nil nil))))
		       (if (and section
				(eq (caar section) 'plain-list)
				(eq (car (caddar section)) 'item)
				(eq (caaddr (caddar section)) 'paragraph)
				;; "Closing note" appears to be hardcoded in org. 
				(string= (caddr (caddr (caddar section))) "CLOSING NOTE "))
			   (goto-char (plist-get (cadar section) :end))
			 (when (eobp) (insert "\n"))
			 (point))))

		   (defcustom company-date-history-length 5
		     "How many dates are stored in the history?")

		   (defun company-date--push-to-history (arg)
		     "save to history"
		     (setq company-date--past-dates
			   (append (list arg)
				   (progn 
				     (setq company-date--past-dates (remove arg company-date--past-dates))
				     (if (> (length company-date--past-dates) company-date-history-length)
					 (seq-subseq company-date--past-dates
						     0 (1- company-date-history-length))
				       company-date--past-dates)))))

		   (defvar company-date-bound-func #'point-at-bol
		     "Function to set the bound for `looking-back'.
There is undoubtedly a better candidate than `point-at-bol'.")
		   (setq company-date-bound-func (lambda () (unless (< (- (point) 10) 1)
							      (- (point) 10) 1)))

		   (defun company-date (command &optional arg &rest ignored)
		     "company backend"
		     (cl-case command
		       (interactive (company-begin-backend 'company-date))
		       (prefix
			(save-match-data
			  (when (looking-back
				 company-date-re
				 (funcall company-date-bound-func))
			    (match-string 0))))
		       (candidates
			(let* ((cands (if (string= (concat company-date-prefix " ") arg)
					  company-date--past-dates
					(company-date--triage-input arg)))
			       (more-cands (company-date--create-other-time-candidates (car cands))))
			  (cond ((and cands more-cands) (append cands (-list more-cands)))
				(cands cands)
				(t nil))))
		       (post-completion
			(cond ((s-starts-with-p "DEADLINE: " arg)
			       (delete-char (* -1 (length arg)))
			       (org-deadline nil (car (s-split "DEADLINE: " arg t))))
			      ((s-starts-with-p "SCHEDULED: " arg)
			       (delete-char (* -1 (length arg)))
			       (org-schedule nil (car (s-split "SCHEDULED: " arg t))))
			      (t
			       (company-date-add-timestamp arg))))

		       (sorted t)
		       (no-cache t)))

		   (defun company-date--normalize-input (string)
		     "Find and replace:
yesterday -> day of week name 
tomorrow"
		     (->> string
			  (downcase)
			  (s-chop-prefix company-date-prefix)
			  (replace-regexp-in-string
			   "yes\\(?:terday\\)?\\.?[[:space:]]"
			   (concat
			    (ts-format "%-Y-%m-%d"	;
				       (ts-adjust 'day -1 (ts-now)))
			    " "))
			  (replace-regexp-in-string
			   "tom\\(?:orrow\\)?\\.?[[:space:]]?"
			   (concat 
			    (ts-day-of-week-name (ts-adjust 'day 1 (ts-now)))
			    " "))))

		   (defun company-date--triage-input (string)
		     (setq string (company-date--normalize-input string))
		     (cond ((s-contains-p "+" string)
			    (company-date--added-time-input string))
			   ((s-contains-p " to " string)
			    (company-date--date-range-input string))
			   (t (company-date--single-date-input string))))

		   (defun company-date--single-date-input (string)
		     "deal with not adding and not to"
		     (let* ((time (org-read-date t t string))
			    (time? (company-date--string-contains-time-p string))
			    (timestamp (company-date--buffer-mod-to-string nil
					 (org-insert-time-stamp time time?))))
		       (company-date--buffer-mod-to-string t
			 (insert timestamp
				 "\n")
			 (org-insert-time-stamp
			  time time? t)
			 (insert "\n")
			 (company-date--create-deadline-and-scheduled-candidates timestamp))))

		   (defmacro company-date--buffer-mod-to-string (separate &rest commands)
		     "Run COMMANDS to insert text into a temp buffer and
return the buffer-string."
		     (declare (indent defun))
		     `(let ((xxx (with-temp-buffer
				   ,@(cl-loop for each in commands
					      collect each)
				   (buffer-substring-no-properties (point-min) (point-max)))))
			(if ',separate
			    (s-split "\n" xxx t)
			  xxx)))

		   (defun company-date--date-range-input (string)	;
		     "Deal with to operations.  Return a list of timestamp strings."
		     (pcase-let* ((`(,from ,to) (s-split " to " string t))
				  (to (company-date--buffer-mod-to-string nil
					(org-insert-time-stamp 
					 (if (string-match
					      (rx bol
						  (** 1 2 digit)
						  (or (seq ":" (= 2 digit))
						      (or "am" "pm")))
					      to)
					     (org-read-date t t
							    (concat (car (s-split " " from))
								    " " to)) ;
					   (org-read-date t t to))
					 (company-date--string-contains-time-p to))))
				  (from (company-date--buffer-mod-to-string nil
					  (org-insert-time-stamp 
					   (org-read-date t t from)
					   (company-date--string-contains-time-p from)))))
		       (cond ((string= (substring to 0 11)
				       (substring from 0 11)) 
			      (let ((hhmm (progn (string-match (rx (** 1 2 digit)
								   (or (seq ":" (= 2 digit))
								       (or "am" "pm")))
							       to)
						 (match-string 0 to))))
				(company-date--buffer-mod-to-string t 
				  (insert from)
				  (backward-char 1)
				  (insert "-")
				  (insert hhmm)
				  (end-of-line)
				  (let ((tt (buffer-substring (1+ (point-at-bol))  (1- (point-at-eol)))))
				    (insert "\n["
					    tt
					    "]")))))

			     (t
			      (company-date--buffer-mod-to-string t
				(insert from
					"--"
					to
					"\n"
					"["
					(substring from 1 -1)
					"]"
					"--"
					"["
					(substring to 1 -1)
					"]"))))))


		   (defun company-date--string-contains-time-p (string)
		     "is there a time specification in the string?"
		     (string-match (rx (** 1 2 digit)
				       (or 
					(seq ":"
					     (** 1 2 digit))
					(or "am" "pm")))
				   string))

		   (defun company-date--added-time-input (string)
		     "Deal with input with a + and return a ts object"
		     (cond ((s-contains-p "+" string)
			    (pcase-let*
				((`(,date . ,adjustments) (s-split "+" string t))
				 (date (ts-parse (car (company-date--triage-input date))))
				 (adjustments
				  (cl-loop for adjustment in adjustments
					   for num = (--> adjustment
							  (s-trim it)
							  (substring it 0 -1)
							  (string-to-number it))
					   for unit = (pcase-exhaustive (substring
									 (s-trim adjustment)
									 -1)
							("d" 'day)
							("m" 'month)
							("y" 'year))
					   append (list unit num)))
				 (timestamp (company-date--buffer-mod-to-string nil
					      (org-insert-time-stamp 
					       (org-read-date nil t 
							      (ts-format "<%Y-%m-%d %H:%M" 
									 (apply #'ts-adjust (append adjustments (list date)))))
					       (company-date--string-contains-time-p string)))))
			      (company-date--triage-input timestamp)))))

		   (defun company-date--create-deadline-and-scheduled-candidates (active-timestamp)
		     "asdf"
		     (insert (concat "DEADLINE: " active-timestamp)
			     "\n"
			     (concat "SCHEDULED: " active-timestamp)))


		   (provide 'company-date)



