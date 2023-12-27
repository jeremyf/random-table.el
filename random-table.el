;;; random-table.el --- Roll on some tables. -*- lexical-binding: t -*-

;;; Metadata
;;
;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1"))
;;
;;; License
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:

(cl-defstruct random-table
  "The definition of a structured random table.

I roll the dice, filter the results, and fetch from the table.
The `random-table/evaluate/table' defines the steps we take to
\"roll on the table.\"

The slots are:

- :name :: the human readable and reference-able name (used for
	   completing read and the key for the table storage).
- :data :: the tabular data, often as a list of strings.  By
	   design, those list of strings can have interpolation
	   (e.g. \"${2d6}\" both of dice structures but also of
	   other tables.
- :roller :: this is what we roll, see `random-table/roll-on'
- :filter :: function to filter the list of dice.

- :fetcher :: function that takes two positional arguments (see
	      `random-table/fetcher/default'.); it is used to
	      fetch the correct entry from the table.
- :exclude-from-prompt :: when true, ignore the prefix arg for
			  prompting for dice roll. (see
			  `random-table/roller')
- :private :: when true, do not show in list of rollable tables.

- :store :: When non-nil, we store the roller's value for the
	    duration of the table evaluation.  Useful for when
	    you have one roll that you use for multiple tables.
- :reuse :: the :name of a table's stored dice results.

About :reuse and :store

There are cases where we want to use one set of dice roles.  For
example, in the \"Oracle (Black Sword Hack)\" table we roll dice
and use those dice results to determine both the answer as well
as whether there are unexpected events.  All from the same roll."
  name
  data
  (roller #'random-table/roller/default)
  ;; TODO: Filter should take a table
  (filter #'random-table/filter/default)
  ;; TODO: Fetcher should take a table
  (fetcher #'random-table/fetcher/default)
  (exclude-from-prompt nil)
  (private nil)
  (store nil)
  (reuse nil))

(cl-defun random-table/register
    (&rest kws &key name data exclude-from-prompt &allow-other-keys)
  "Store the DATA, NAME, and all given KWS in a `random-table'."
  ;; We need to guard for a reserved character; which we use for operations.
  (if (string-match-p "[{}\]\[)(/\+\-\*]" name)
      (user-error (concat "Attempt to register \"%s\" table failed.  "
			  "You cannot include the following characters:  "
			  "\"{\", \"}\", \"[\", \"]\", \"(\", \")\", \"/\", "
			  "\"*\", \"-\", \"+\".")
		  name)
    (let* ((struct (apply #'make-random-table
			  :name name
			  ;; When there's only one possible result, don't prompt
			  ;; the user when they chose the "I'll roll my own
			  ;; dice" option.
			  :exclude-from-prompt (or exclude-from-prompt
						   (= 1 (length (-list data))))
			  :data (-list data) kws)))
      (puthash name struct random-table/storage/tables))))

(defun random-table/coerce-input (text)
  "Coerce initial TEXT to wrap in \"{\" \"}\" brackets."
  (if (string-match-p "{" text) text (concat "{" text "}")))

;;;; Interactive
;;;###autoload
(defun random-table/roll (text)
  "Evaluate the given TEXT by \"rolling\" it.

This can either be a named table or a general text (e.g. 2d6).
Or a combination of multiple tables.

When you pass the universal prefix arg, you'll be prompted to
physically roll dice for the various tables.

When you pass \"2d6\" and pass the universal prefix arg, you will
not be prompted to roll \"2d6\" dice, it rolls that.  In other
words, giving dice expressions in text will not prompt you to
roll them.

We report that function via `random-table/reporter'.

With each invocation of `random-table/roll' we assign a new empty
hash table to `random-table/storage/results'."
  (interactive (list (completing-read "Expression: "
				      random-table/storage/tables
				      ;; Predicate that filters out non-private
				      ;; tables.
				      (lambda (name table &rest args)
					(not (random-table-private table))))))
  (let ((random-table/storage/results (make-hash-table :test 'equal)))
    (funcall random-table/reporter
	     text
	     (random-table/parse (random-table/coerce-input text)))))

;;;###autoload
(defun random-table/roll-region (&optional prefix)
  "Roll region or current line.

When PREFIX is given replace the marked text."
  (interactive "P")
  (let ((random-table/reporter/format-function (lambda (e r) (format "%s" r)))
	(random-table/reporter #'random-table/reporter/as-kill-and-message)
	(text (random-table/coerce-input
	       (if (region-active-p)
		  (buffer-substring-no-properties
		   (region-beginning) (region-end))
		(apply #'buffer-substring-no-properties
		       (save-excursion
			 (goto-char (point-at-bol))
			 (skip-syntax-forward " " (point-at-eol))
			 (let ((beg (point)))
			   (goto-char (point-at-eol))
			   (skip-syntax-backward " " (point-at-bol))
			   (list beg (point))))))))
	(current-prefix-arg nil))
    (let ((result (random-table/roll text)))
      (when (and prefix (region-active-p))
	  (delete-region (region-beginning) (region-end))
	  (insert result)))))

(cl-defun random-table/prompt (name &key type range default)
  "Prompt for the given NAME.

Re-use the cached prompted answer or use the
`random-table/prompt/registry' to evaluate the prompt; then cache
that result."
  (if type
      (random-table/prompt/put name
			       (let ((prompt (format "%s: " name)))
				 (cond
				  ((eq type 'bound-integer-range)
				   `(random-table/completing-read/integer-range
				     ,prompt ,range))
				  ((eq type #'read-number)
				   `(read-number ,prompt ,default))
				  ((eq type #'completing-read)
				   `(random-table/completing-read/alist
				     ,prompt ,range nil t))
				  (t (user-error
				      "Unknown type %s function for %s registry"
				      type name)))))
    (let ((value (or (random-table/storage/results/get-rolled-value name)
		     (apply (random-table/prompt/get name)))))
      (random-table/storage/results/put-rolled-value name value)
      value)))

(defvar random-table/prompt/registry
  (make-hash-table :test 'equal)
  "Stores the prompts registered by `random-table/prompt/register'.")

(defun random-table/prompt/get (name)
  (gethash name random-table/prompt/registry))

(defun random-table/prompt/put (name value)
  (puthash name value random-table/prompt/registry))

(cl-defmacro random-table/create-text-replacer-function
    (docstring &key name replacer regexp)
  "Create NAME function as a text REPLACER for REGEXP.

- NAME: A symbol naming the replacer function.
- REPLACER: A lambda with a number of args equal to one plus the number of
	    capture regions of the REGEXP.  The first parameter is the original
	    text, the rest are the capture regions of the REGEXP.
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
		       ;; Convert the matched data results into a list, with the
		       ;; `car' being the original text and the `cdr' being a
		       ;; list of each capture region.
		       (mapcar (lambda (i) (match-string i md))
			       (number-sequence 0 (- (/ (length (match-data)) 2)
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

- \"[dog/cat/horse]\" (e.g. 3 entries)
- \"[hello world/good-bye mama jane]\" (2 entries)

This skips over inner tables that have one element (e.g. [one])."
 :name random-table/text-replacer-function/inner-table
 :regexp "\\[\\([^\]]+/[^\]]+\\)\\]"
 :replacer (lambda (matching-text inner-table)
	     (seq-random-elt (s-split "/" inner-table))))

(random-table/create-text-replacer-function
 "Conditionally perform math operation on table results for TEXT.

Examples of math operation:

- \"{(Henchman > Morale Base) + (Henchman > Morale Variable)}\""
 :name random-table/text-replacer-function/table-math
 :regexp "{(\\([^)]*\\))\s*\\([\-+\*]\\)\s*(\\([^)]*\\))}"
 :replacer (lambda (matching-text left-operand operator right-operand)
	     (format "%s" (funcall
			   (intern operator)
			   (string-to-number
			    (random-table/parse
			     (string-trim left-operand)))
			   (string-to-number
			    (random-table/parse
			     (string-trim right-operand)))))))

(defvar random-table/current-roll
  nil
  "The most immediate and current stored roll.")

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with the current roll.

Examples of current roll:

- \"{ CURRENT_ROLL }\"
- \"{CURRENT_ROLL}\"

See `random-table/current-roll'."
 :name random-table/text-replacer-function/current-roll
 :regexp "{\\(\s*CURRENT_ROLL\s*\\)}"
 :replacer (lambda (matching-text current)
	     (or random-table/current-roll matching-text)))

(random-table/create-text-replacer-function
 "Conditionally replace dice-expression of TEXT.

Examples:

- \"{1d6}\"
- \"{2d6 + 3}\"
- \"{ d6+3 }\"
"
 :name random-table/text-replacer-function/dice-expression
 :regexp "{\s*\\([1-9][[:digit:]]*d[[:digit:]]+\\)\s*\\([+-][0-9]+\\)?\s*}"
 :replacer (lambda (matching-text dice &optional modifier)
	     (format "%s" (random-table/dice/roll (concat dice modifier)))))

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with roll on table.

The regexp will match the entire line and attempt a direct lookup
on the tables; failing that it will attempt to evaluate as a dice expression

See `random-table/dice/regex' for matching dice expressions."
 :name random-table/text-replacer-function/from-interactive-prompt
 :regexp "^\\(.+\\)$"
 :replacer (lambda (matching-text table-name)
	     (if-let ((table (random-table/fetch
			      (string-trim table-name) :allow_nil t)))
		 (random-table/evaluate/table table)
	       (if (string-match-p random-table/dice/regex
				   (string-trim matching-text))
		   (random-table/dice/roll (string-trim matching-text))
		 matching-text))))

(random-table/create-text-replacer-function
 "Conditionally replace TEXT with roll on table.

Examples:

- \"{Name (d2)}\"."
 :name random-table/text-replacer-function/named-table
 :regexp "{\s*\\([^})]+\\)\s*\\((\\([^)]+\\))\\)?\s*}"
 :replacer (lambda (matching-text table-name &optional has-roller roller)
	     (if-let ((table (random-table/fetch
			      (string-trim table-name) :allow_nil t)))
		 (random-table/parse
		  (random-table/evaluate/table table roller))
	       matching-text)))

(defcustom random-table/text-replacer-functions
  '(random-table/text-replacer-function/current-roll
    random-table/text-replacer-function/dice-expression
    random-table/text-replacer-function/named-table
    random-table/text-replacer-function/inner-table
    random-table/text-replacer-function/table-math)
  "Functions take on string parameter and return a string.

The function is responsible for finding and replacing any matches
within the text.

See `random-table/create-text-replacer-function' macro for
creating one of these functions."
  :group 'random-table
  :package-version '(random-table . "0.4.0")
  :type '(list :value-type (group function)))

(defun random-table/parse (text)
  "Roll the given TEXT.

This is done by formatting the given text and passing it to each
of the functions listed in `random-table/text-replacer-functions'."
  (let ((given-text (format "%s" text)))
    (cl-reduce (lambda (string el) (funcall el string))
	       random-table/text-replacer-functions
	       :initial-value given-text)))

(defvar random-table/storage/results
  (make-hash-table :test 'equal)
  "An ephemeral storage for various results of rolling.

As part of the rolling, we both add to and remove those stored
values; that is to say functions are responsible for clean-up.
See `random-table' for discussion about storage and reuse.")

(defun random-table/storage/results/put-rolled-value (name value)
  (puthash name value random-table/storage/results))

(defun random-table/storage/results/get-rolled-value (name)
  (gethash name random-table/storage/results))

(defvar random-table/dice/regex
  "^\\([0-9]*\\)?d\\([0-9]*\\)\\([+-][0-9]*\\)?")

;;; Dice String Evaluator
;;
;; The following code (with the function name prefix of \"random-table/dice\"
;; is derived from Pelle Nilsson's decide.el package
(defun random-table/dice/roll (spec-string)
  "Evaluate the given SPEC-STRING by parsing as a dice expression."
  (if (string= "d66" spec-string)
      (+ (* 10 (+ 1 (random 6))) (+ 1 (random 6)))
    (apply #'random-table/dice/roll-spec
	   (random-table/dice/parse-spec spec-string))))

(defun random-table/dice/parse-spec (spec)
  "Convert SPEC to list:

   - Number of dice
   - Face
   - Adder

  e.g. \"1d6\" -> (1 6 0) or \"2d10+2\" -> (2 10 2)"
  (when (string-match
	 "^\\([0-9]*\\)?d\\([0-9]*\\)\\([+-][0-9]*\\)?"
	 spec)
    (list (random-table/dice/string-to-number
	   (match-string 1 spec) 1)
	  (random-table/dice/string-to-number
	   (match-string 2 spec) 6)
	  (random-table/dice/string-to-number
	   (match-string 3 spec) 0))))

(defun random-table/dice/string-to-number (spec default)
  "Convert the SPEC (and DEFAULT) into an integer."
  (let ((n (if (stringp spec)
	       (string-to-number spec)
	     0)))
    (cond ((null spec) default)
	  ((> n 0) n)
	  ((string= "" spec) default)
	  ((string= "+" spec) 0)
	  ((string= "-" spec) 0)
	  (t spec))))

(defun random-table/dice/roll-spec (number-dice faces modifier)
  "Roll the NUMBER-DICE each with FACES number of sides and add MODIFIER."
  (let ((amount modifier))
    (dotimes (i number-dice)
      (setq amount (+ amount 1 (random faces))))
    amount))

(defcustom random-table/reporter
  #'random-table/reporter/as-kill-and-message
  "The function takes two positional parameters:

- EXPRESSION :: The text to evaluate for \"rolling\"
- RESULT :: The results of those rolls.

See `random-table/reporter/as-kill-and-message'."
  :group 'random-table
  :package-version '(random-table . "0.1.0")
  :type '(choice
	  (function-item :tag "Kill and Message"
			 random-table/reporter/as-kill-and-message)
	  (function-item :tag "Insert"
			 random-table/reporter/as-insert)))

(defvar random-table/reporter/format-function
  (lambda (expression results) (format "- %s :: %s" expression results))
  "The configured function takes two positional arguments:

- expression :: the initial text provided `random-table/roll'
- restults :: the transformed results by replacing the table declarations with
	      their rolled results.

I structure my results in an `org-mode' definition list format.")

(defun random-table/reporter/as-kill-and-message (expression results)
  "Report RESULTS of EXPRESSION as `message' and `kill'.

See `random-table/reporter'."
  (let ((text (funcall random-table/reporter/format-function
		       expression results)))
    (kill-new text)
    (message text)))

(defun random-table/reporter/as-insert (expression results &optional buffer)
  "Insert RESULTS of EXPRESSION into BUFFER.

See `random-table/reporter'."
  (with-current-buffer (or buffer (current-buffer))
    (end-of-line)
    (insert (funcall random-table/reporter/format-function
		     expression results))
    (insert "\n")))

(cl-defun random-table/fetch (value &key allow_nil)
  "Coerce the given VALUE to a registered `random-table'.

When the given VALUE cannot be found in the
`random-table/stroage/tables' registry we look to ALLOW_NIL.

When ALLOW_NIL is non-nil, we return nil when no table is found
in `random-table/stroage/tables' registry.

When ALLOW_NIL is nil we raise an `error' when no table was
found in the `random-table/stroage/tables' registry."
  (if-let ((table (cond
		   ((random-table-p value)
		    value)
		   ((stringp value)
		    (gethash value random-table/storage/tables))
		   ((integerp value)
		    nil)
		   (t
		    (error (concat "Expected %s to be a `random-table', "
				   "`symbol', `integer', or `string' got %s")
			   value
			   (type-of value))))))
      table
    (unless allow_nil
      (error "Could not find table %s; use `random-table/register'" value))))

(defvar random-table/storage/tables
  (make-hash-table :test 'equal)
  "A hash-table of random tables.

The hash key is the \"human readable\" name of the table (as a symbol).
The hash value is the contents of the table.")

(defun random-table/roll-on (table &optional roller)
  "Roll on the TABLE with the ROLLER.

When no ROLLER is specified, use `random-table-roller' to find
the configured roller.

See `random-table'."
  (if-let ((roller (or roller (random-table-roller table))))
      (cond
       ((functionp roller) (funcall roller table))
       ((stringp roller) (random-table/roller/string roller))
       ((seqp roller) (random-table/roller/seq roller))
       (_ (user-error "Unable to handle %S roller for %s table"
		      roller
		      (random-table-name table))))
    (user-error "Expected given %s to have roller; got nil"
		(random-table-name table))))

(defun random-table/roller/default (table)
  "Randomly roll on the TABLE."
  ;; Constant off by one errors are likely
  (let ((faces (length (-list (random-table-data table)))))
    (if (and current-prefix-arg
	     (not (random-table-exclude-from-prompt table)))
	(read-number (format "Roll 1d%s for %s: "
			     faces (random-table-name table)))
      (+ 1 (random faces)))))

(defun random-table/roller/string (text)
  "Interpolate given TEXT as a roller."
  (if (or (string= "d66" (string-trim text))
	  (string-match-p random-table/dice/regex text))
      (if current-prefix-arg
	  (read-number (format "Roll %s: " text))
	(string-to-number
	 (format "%s" (random-table/dice/roll (string-trim text)))))
    (random-table/parse text)))

(defun random-table/roller/seq (seq)
  "Interpolate given SEQ as a roller."
  (let ((func (car seq))
	(rolls (mapcar
		(lambda (text)
		  (let ((value (if (random-table/prompt/get text)
				   (random-table/prompt text)
				 (random-table/roller/string text))))
		    (string-to-number (format "%s" value))))
		(cdr seq))))
    (apply func rolls)))

(defun random-table/filter/default (&rest rolls)
  "Filter the given ROLLS and return an integer.

See `random-table/roller/default'."
  (cond
   ;; Allows us to have table entries that are named.
   ((stringp (car rolls)) (car rolls))
   (t (apply #'+ (-list rolls)))))

(defun random-table/fetcher/default (data &optional roll)
  "Find ROLL on the given table's DATA.

When ROLL is not given, choose a random element from the TABLE."
  (if-let ((index (if (integerp roll) roll (car roll))))
      ;; Sniff out if the first element to see if we're dealing with a table
      ;; that has ranges.
      (if (-cons-pair? (car data))
	  ;; We have a cons-pair, meaning we have multiple rolls mapping to the
	  ;; same result.
	  (cdr (seq-find
		(lambda (row)
		  (if (-cons-pair? row)
		      (let ((range (car row)))
			(cond
			 ((-cons-pair? range)
			  (and (>= index (car range)) (<= index (cdr range))))
			 ((listp range)
			  (member index range))
			 ((integerp range)
			  (= index range))
			 ((stringp range)
			  (string= index range))
			 (t
			  (error (concat "Expected `cons', `list', `string' or "
					 "`integer' got %s for row %S.")
				 (type-of range) row))))
		    (member index (car row))))
		data))
	;; Off by one errors are so very real.
	(nth (- index 1) data))
    (seq-random-elt data)))


(defun random-table/evaluate/table (table &optional roller)
  "Evaluate the random TABLE, optionally using the given ROLLER.

See `random-table' structure."
  (let ((random-table/current-roll (random-table/evaluate/table/roll table roller)))
    ;; TODO: This is wildly naive.  Perhaps the current_roll needs to be
    ;; replaced with the "${Current Roll for [My Tablename]}".  Then we can
    ;; Cache that rolled value and retrieve it.
    (random-table/evaluate/table/fetch-rolled-value table random-table/current-roll)))

(defun random-table/evaluate/table/roll (table &optional roller)
  "Roll on the TABLE, conditionally using ROLLER.

This function favors re-using and caching values.

Why cache values?  Some tables you roll one set of dice and then
use those dice to lookup on other tables."
  (let ((results
	 (or (when-let ((reuse-table-name (random-table-reuse table)))
	       (or
		(random-table/storage/results/get-rolled-value reuse-table-name)
		(random-table/roll-on (random-table/fetch reuse-table-name) roller)))
	     (random-table/roll-on table roller))))
    (when (random-table-store table)
      (random-table/storage/results/put-rolled-value
       (random-table-name table) results))
    results))

(defun random-table/evaluate/table/fetch-rolled-value (table rolled)
  "Fetch the ROLLED value from the TABLE's :data slot."
  (let* ((table (random-table/fetch table))
	 (data (random-table-data table))
	 (filtered (apply (random-table-filter table) (-list rolled)))
	 (row (apply (random-table-fetcher table) data (-list filtered))))

    (or (when row (random-table/parse row)) "")))

(defun random-table/completing-read/alist (prompt alist &rest args)
  "Like `completing-read' but PROMPT to find value in given ALIST.

ARGS are passed to `completing-read'."
  (alist-get (apply #'completing-read prompt alist args)
	     alist nil nil #'string=))

(defun random-table/completing-read/integer-range (prompt range)
  "Like `completing-read' but PROMPT to find integer value in RANGE."
  (let ((strings (mapcar #'number-to-string range)))
    (string-to-number (completing-read prompt strings nil t))))



(defun random-table/roll/test-all ()
  "A convenience function to test all of the public `random-table' entries."
  (maphash (lambda (key table)
	     (unless (random-table-private table)
	       (message "Testing %s table" key)
	       ;; The test does not call these interactively, but the methods
	       ;; assume a current-prefix-arg
	       (funcall #'random-table/roll (random-table-name table))))
	   random-table/storage/tables))

(provide 'random-table)
;;; random-table.el ends here
