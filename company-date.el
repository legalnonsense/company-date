;; TODO: add parsing for repeaters

(require 'ts)

(defvar company-date-modes '(org-mode)
  "List of major modes which will utilize `company-date'.")

(defvar company-date-prefix "<<"
  "Prefix to invoke date completion.")

(defvar company-date-termination-suffix ">"
  "Termination suffix. If this string is found after `company-date-prefix'
then company will not attempt to complete.")

(defvar company-date-re (concat company-date-prefix
				"[[:alnum:]]"
				"[[:alnum:][:space:]:./-]+"
				(when company-date-termination-suffix
				  (concat "[^" company-date-termination-suffix "]")))

  "RE to trigger a completion.")

(defvar company-date-processed-result nil)

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
       (and (memq major-mode company-date-modes)
	    (looking-back company-date-re (funcall company-date-bound-func))
	    (let* ((match (match-string-no-properties 0))
		   (processed-match (->> match
					 (downcase)
					 (s-chop-prefix "<")
					 ;; convert any customized user input to a form that `org-read-date' allows
					 ;; specifically here, allow "yes" "yesterday" to propose to fill the previous date
					 ;; and "tom" or "tomorrow" fill the next. E.g. "<Tom at 5pm" will propose
					 ;; <2021-06-08 Tue 17:00> which is tomorrow's date today. 
					 (replace-regexp-in-string "yes\\(?:terday\\)?\\.?[[:space:]]"
								   (concat 
								    (ts-day-of-week-name (ts-adjust 'day -1 (ts-now)))
								    " "))
					 (replace-regexp-in-string "tom\\.?[[:space:]]?"
								   (concat 
								    (ts-day-of-week-name (ts-adjust 'day 1 (ts-now)))
								    " ")))))
	      ;; This was a way to check against dates that didn't make sense to avoid
	      ;; default filling any text with today's date.
	      ;; Since `org-read-date' accepts a lot more than `parse-time-string' it does
	      ;; not make sense to use go back to parse-time-string to putting out useless completions
	      ;; of today's date for anything following < without a space.
	      ;; A more indicative prefix may be appropriate.

	      ;; it could, however, be a test for additional test before generating the match
	      ;; (unless (equal (parse-time-string processed-match)
	      ;; 		  '(nil nil nil nil nil nil nil -1 nil))
	      
	      ;; We have to save this value because when company enters the text,
	      ;; it will automatically delete the original buffer text and substitute the replacement.  
	      ;; Since `processed-match' does not reflect the original buffer text, it can't be used
	      ;; to manage the buffer text

	      ;; I do not know how to avoid this without resorting to a global variable
	      ;; (I tried returning match as a cons cell of the original and modified text,
	      ;; but I think that company is hardwired for it to be a string.  
	      (setq company-date-processed-result processed-match)
	      
	      match))))
    (candidates
     ;; this inserts the match selected by the user. It automatically deletes the entered text,
     ;; and replaces it with the new text. 

     (with-temp-buffer
       (let*
	   ;; HACK: I just wanted to get it done.  It's not pretty. 
	   ((result (if (s-contains-p " to " company-date-processed-result)
			(s-split " to " company-date-processed-result)
		      company-date-processed-result))
	    (date (if (listp result)
		      (cl-loop for r in result
			       collect (org-read-date t t r))
		    (org-read-date t t result))))
	 (if (listp result)
	     (progn
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
	       (insert "\n"))
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



