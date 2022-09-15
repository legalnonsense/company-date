;; TODO: add parsing for repeaters

(require 'ts)

(defcustom company-date-prefix "<<"
  "Prefix to invoke date completion.")

(defcustom company-date-termination-suffix ">>"
  "Termination suffix. If this string is found after `company-date-prefix'
then company will not attempt to complete.")

(defconst company-date-re (concat company-date-prefix
				  "[[:alnum:]]"
				  "[[:alnum:][:space:]:./-]+"
				  (when company-date-termination-suffix
				    (concat "[^" company-date-termination-suffix "]")))

  "RE to trigger a completion.")

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
     ;; I don't know if I have to do this, but it can't hurt
     (save-match-data 
       (and (looking-back company-date-re (funcall company-date-bound-func))
	    (let* ((match (match-string-no-properties 0))
		   (processed-match (->> match
					 (downcase)
					 (s-chop-prefix company-date-prefix)
					 (replace-regexp-in-string "yes\\(?:terday\\)?\\.?[[:space:]]"
								   (concat 
								    (ts-day-of-week-name (ts-adjust 'day -1 (ts-now)))
								    " "))
					 (replace-regexp-in-string "tom\\.?[[:space:]]?"
								   (concat 
								    (ts-day-of-week-name (ts-adjust 'day 1 (ts-now)))
								    " ")))))	      
	      (setq company-date--processed-result processed-match)
	      match))))
    (candidates
     ;; this inserts the match selected by the user. It automatically deletes the entered text,
     ;; and replaces it with the new text. 

     (with-temp-buffer
       (let*
	   ;; HACK: I just wanted to get it done.  It's not pretty. 
	   ((result (if (s-contains-p " to " company-date--processed-result)
			(s-split " to " company-date--processed-result)
		      company-date--processed-result))
	    (date (if (listp result)
		      (cl-loop for r in result
			       when (not (string-match "[^[:digit:]APMapm]" (cadr result)))
			       do (setf (cadr result) (concat (car (s-split " " (car result)))
							      " " 
							      (cadr result)))
			       collect (org-read-date t t r))
		    (org-read-date t t result))))
	 (if (listp result)
	     (progn 
	       (let ((date1 (company-date--buffer-mod-to-string
			     (org-insert-time-stamp (car date) nil)))
		     (date2 (company-date--buffer-mod-to-string
			     (org-insert-time-stamp (cadr date) nil))))
		 (cond ((string= date1 date2)
			(org-insert-time-stamp (car date) t nil)
			(let ((hhmm (--> (company-date--buffer-mod-to-string
					  (org-insert-time-stamp (cadr date) t))
					 (ts-parse-org it)
					 (concat
					  (s-pad-left 2 "0"
						      (number-to-string (ts-hour it)))
					  ":"
					  (s-pad-left 2 "0"
						      (number-to-string (ts-minute it)))))))
			  (backward-char 1)
			  (insert "-")
			  (insert hhmm)
			  (end-of-line)
			  (insert "\n")
			  (org-insert-time-stamp (car date) t t)
			  (let ((hhmm (--> (company-date--buffer-mod-to-string
					    (org-insert-time-stamp (cadr date) t))
					   (ts-parse-org it)
					   (concat
					    (s-pad-left 2 "0"
							(number-to-string (ts-hour it)))
					    ":"
					    (s-pad-left 2 "0"
							(number-to-string (ts-minute it)))))))
			    (backward-char 1)
			    (insert "-")
			    (insert hhmm)
			    (end-of-line)
			    (insert "\n"))))
		       (t
			(org-insert-time-stamp (car date) nil nil)
			(insert "--")
			(org-insert-time-stamp (cadr date) nil nil)
			(insert "\n")
			(org-insert-time-stamp (car date) t nil)
			(insert "--")
			(org-insert-time-stamp (cadr date) t nil)
			(insert "\n")
			(org-insert-time-stamp (car date) nil t)
			(insert "--")
			(org-insert-time-stamp (cadr date) nil t)
			(insert "\n")
			(org-insert-time-stamp (car date) t t)
			(insert "--")
			(org-insert-time-stamp (cadr date) t t)
			(insert "\n")))))
	   (org-insert-time-stamp date nil nil)
	   (insert "\n")			       
	   (org-insert-time-stamp date t nil)
	   (insert "\n")
	   (org-insert-time-stamp date nil t)
	   (insert "\n")
	   (org-insert-time-stamp date t t)
	   (insert "\n"))
	 (split-string (buffer-string)
		       "\n"))))
    (post-completion
     (insert " "))
    (sorted t)
    (no-cache t)))


(provide 'company-date)


(defmacro company-date--buffer-mod-to-string (&rest commands)
  `(with-temp-buffer
     ,@(cl-loop for each in commands
		collect each)
     (buffer-substring-no-properties (point-min) (point-max))))



