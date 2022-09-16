;; TODO: add parsing for repeaters

(require 'ts)

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

(defun company-date--push-to-history (arg)
  "save to history"
  (setq company-date--past-dates
	(append (list arg)
		(progn 
		  (setq company-date--past-dates (remove arg company-date--past-dates))
		  (if (> (length company-date--past-dates) 5)
		      (seq-subseq company-date--past-dates
				  0 4)
		    company-date--past-dates)))))

(defvar company-date--processed-result nil)

(defvar company-date-bound-func #'point-at-bol
  "Function to set the bound for `looking-back'.
There is undoubtedly a better candidate than `point-at-bol'.")
(setq company-date-bound-func (lambda () (unless (< (- (point) 10) 1)
					   (- (point) 10) 1)))

(defun company-date (command &optional arg &rest ignored)
  (cl-case command
    (interactive (company-begin-backend 'company-date))
    (prefix
     (save-match-data
       (when (looking-back
	      company-date-re
	      (funcall company-date-bound-func))
	 (match-string 0))))
    (candidates
     (setq xxx arg)
     (if (string= "<< " arg)
	 company-date--past-dates
       (time-parser--triage arg)))
    (post-completion
     (cond ((s-starts-with-p "DEADLINE: " arg)
	    (delete-char (* -1 (length arg)))
	    (org-deadline nil (car (s-split "DEADLINE: " arg t))))
	   ((s-starts-with-p "SCHEDULED: " arg)
	    (delete-char (* -1 (length arg)))
	    (org-schedule nil (car (s-split "SCHEDULED: " arg t))))
	   (t
	    (company-date--push-to-history arg)
	    (insert " "))))
    (sorted t)
    (no-cache t)))

(defun time-parser--normalize-input (string)
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

(defun time-parser--triage (string)
  (setq string (time-parser--normalize-input string))
  (cond ((s-contains-p "+" string)
	 (time-parser--adder string))
	((s-contains-p " to " string)
	 (time-parser--toer string))
	(t (time-parser--single-input string))))

(defun time-parser--single-input (string)
  "deal with not adding and not to"
  (let* ((time (org-read-date t t string))
	 (time? (time-parser--string-contains-time-p string))
	 (timestamp (time-parser--buffer-mod-to-string nil
		      (org-insert-time-stamp time time?))))
    (time-parser--buffer-mod-to-string t
      (insert timestamp
	      "\n")
      (org-insert-time-stamp
       time time? t)
      (insert "\n")
      (time-parser--create-deadline-and-scheduled timestamp))))

(defmacro time-parser--buffer-mod-to-string (separate &rest commands)
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

(defun time-parser--toer (string)	;
  "Deal with to operations.  Return a list of timestamp strings."
  (pcase-let* ((`(,from ,to) (s-split " to " string t))
	       (to (time-parser--buffer-mod-to-string nil
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
		      (time-parser--string-contains-time-p to))))
	       (from (time-parser--buffer-mod-to-string nil
		       (org-insert-time-stamp 
			(org-read-date t t from)
			(time-parser--string-contains-time-p from)))))
    (cond ((string= (substring to 0 11)
		    (substring from 0 11)) 
	   (let ((hhmm (progn (string-match (rx (** 1 2 digit)
						(or (seq ":" (= 2 digit))
						    (or "am" "pm")))
					    to)
			      (match-string 0 to))))
	     (time-parser--buffer-mod-to-string t 
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
	   (time-parser--buffer-mod-to-string t
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


(defun time-parser--string-contains-time-p (string)
  "is there a time specification in the string?"
  (string-match (rx (** 1 2 digit)
		    (or 
		     (seq ":"
			  (** 1 2 digit))
		     (or "am" "pm")))
		string))

(defun time-parser--adder (string)
  "Deal with input with a + and return a ts object"
  (cond ((s-contains-p "+" string)
	 (pcase-let*
	     ((`(,date . ,adjustments) (s-split "+" string t))
	      (date (ts-parse (car (time-parser--triage date))))
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
	      (timestamp (time-parser--buffer-mod-to-string nil
			   (org-insert-time-stamp 
			    (org-read-date nil t 
					   (ts-format "<%Y-%m-%d %H:%M" 
						      (apply #'ts-adjust (append adjustments (list date)))))
			    (time-parser--string-contains-time-p string)))))
	   (time-parser--triage timestamp)))))

(defun time-parser--create-deadline-and-scheduled (active-timestamp)
  "asdf"
  (insert (concat "DEADLINE: " active-timestamp)
	  "\n"
	  (concat "SCHEDULED: " active-timestamp)))
  

(provide 'company-date)



