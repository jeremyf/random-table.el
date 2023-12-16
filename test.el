(cl-defmacro random-table/create-text-replacer-function (docstring &key name replacer regexp)
  "Create NAME function as a text REPLACER for REGEXP.

- NAME: A symbol
- REPLACER: A lambda with a number of args equal to the number of capture
  regions of the REGEXP.
- REGEXP: The regular expression to test against the given text.
- DOCSTRING: The docstring for the newly created function.

This macro builds on the logic found in `s-format'"
  (let ((name (if (stringp name) (intern name) name)))
    `(defun ,name (text)
       ,docstring
       (let ((saved-match-data (match-data)))
	 (unwind-protect
	     (replace-regexp-in-string
	      ,regexp
	      (lambda (md)
		(let ((capture-region-text-list
		       ;; Zip the matching regions (excluding the entire match)
		       (mapcar (lambda (i) (match-string i md))
			       (number-sequence 1 (- (/ (length (match-data)) 2)
						     1))))
		      (replacer-match-data (match-data)))
		  (unwind-protect
		      (let ((replaced-text
                             (cond
                              (t
                               (set-match-data saved-match-data)
			       (apply ,replacer capture-region-text-list)))))
			(if replaced-text
			    (format "%s" replaced-text)
			  (signal 's-format-resolve md)))
		    (set-match-data replacer-match-data))))
	      text t t)
	   (set-match-data saved-match-data))))))

(random-table/create-text-replacer-function
 "Conditionally replace inner-table for TEXT.
Examples of inner-table are:
- [dog/cat/horse] (3 entries)
- [hello world/good-bye mama jane] (2 entries)
This skips over inner tables that have one element (e.g. [one])."
 :name random-table/text-replacer-function/inner-table
 :regexp "\\[\\([^\]]+/[^\]]+\\)\\]"
 :replacer (lambda (inner-table)
             (seq-random-elt (s-split "/" inner-table))))

(random-table/create-text-replacer-function
 "Conditionally replace math operation for TEXT.
Examples of math operation:
- [Henchman > Morale Base] + [Henchman > Morale Variable]"
 :name random-table/text-replacer-function/math-operation
 :regexp "\\[\\(.*\\)\\][[:space:]]*\\(-\\|\\+\\|\\*\\)[[:space:]]*\\[\\(.*\\)\\]"
 :replacer (lambda (left-operand operator right-operand)
             (format "%s" (funcall (intern operator)
				   (string-to-number
				    (random-table/text-reduce left-operand))
				   (string-to-number
				    (random-table/text-reduce right-operand))))))

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with the current roll.

See `random-table/current-roll'."
 :name random-table/text-replacer-function/current_roll
 :regexp "\\[\\(current_roll\\)\\]"
 :replacer (lambda (text)
	     (or random-table/current-roll text)))

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with table and custom roller."
 :name random-table/text-replacer-function/fallback
 :regexp "\\[\\([1-9][[:digit:]]*d[[:digit:]]+\\)[[:space:]]*\\([+-][0-9]+\\)?\\]"
 :replacer (lambda (dice &optional modifier)
	     (format "%s" (random-table/dice/roll (concat dice modifier)))))

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with roll on table."
 :name random-table/text-replacer-function/named-table
 :regexp "\\${[[:space:]]?\\([^})]+\\)[[:space:]]*\\(([^)]+)\\)?[[:space:]]?}"
 :replacer (lambda (table-name &optional roller-expression)
	     (if-let ((table (random-table/fetch
			      (s-trim table-name) :allow_nil t)))
		 (random-table/evaluate/table table roller-expression)
	       text)))

(defvar random-table/text-replacer-functions
  '(random-table/text-replacer-function/current_roll
    random-table/text-replacer-function/named-table
    random-table/text-replacer-function/inner-table
    random-table/text-replacer-function/math-operation
    random-table/text-replacer-function/fallback)
  "Functions that each have one positional string parameter returning a string.

The function is responsible for finding and replacing any matches
within the text.

See `random-table/create-text-replacer-function' macro for creating one of these
functions.")

(defvar random-table/table-name/invalid-characters
  '("[" "]" "{" "}" "/" "(" ")")
  "Characters that should not be used as the name of a `random-table'.")
