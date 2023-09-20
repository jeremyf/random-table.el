;;; random-table.el --- Roll on some tables. -*- lexical-binding: t -*-

;;; Metadata
;;
;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>
;; Version: 0.2
;; Package-Requires: ((s "1.3") (emacs "29.1"))
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
;;; Commentary:
;;
;; A series of blog posts regarding this package:
;;
;; - https://takeonrules.com/series/emacs-random-table-el-package/
;;
;; This package provides a means of registering random tables (see
;; `random-table' and `random-table/register') and then rolling on those tables
;; (see the `random-table/roll').
;;
;; The `random-table/roll' is an `interactive' function that will prompt you to
;; select an expression.  You can choose from a list of registered public tables
;; or provide your own text.  This package uses the `s-format' to parse the
;; given expression.
;;
;; The guts of the logic is `random-table/evaluate/table' namely how we:
;;
;; - Gather the dice (represented by the :roller)
;; - Filter the dice to a value (represented by the :filter), we might pick a
;;          single dice rolled or sum them or whatever.
;; - Fetch the filtered result from the table.
;; - Evaluate the row.
;;
;; Examples:
;; - (random-table/roll "2d6") will roll 2 six-sided dice.
;; - (random-table/roll "There are ${2d6} orcs.") will roll 2 six-sided dice and
;;   output the sentence "There are 7 orcs."
;;
;; Tables can reference other tables, using the above string interpolation
;; (e.g. "Roll on ${Your Table}" where "Your Table" is the name of a registered
;; table.).  No considerations have been made to check for cyclical references,
;; you my dear human reader, must account for that.

;;; Code:

;;;; Requirements:
(require 's)

;;;; Data Structures and Storage and Defaults
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
 (e.g. \"${2d6}\" both of dice structures but also of other
  tables.
- :roller :: this is what we roll, see `random-table/roller/roll'
- :filter :: function to filter the list of dice.
- :fetcher :: function that takes two positional arguments (see
  `random-table/fetcher/default'.); it is used to fetch the correct entry
  from the table.
- :exclude-from-prompt :: when true, ignore the prefix arg for
  prompting for dice roll. (see `random-table/roller')
- :private :: when true, do not show in list of rollable tables.
- :store :: When non-nil, we store the roller's value for the
  duration of the table evaluation.  Useful for when you have one
  roll that you use for multiple tables.
- :reuse :: the :name of a table's stored dice results.

About :reuse and :store

There are cases where we want to use one set of dice roles.  For
example, in the \"Oracle (Black Sword Hack)\" table we roll dice
and use those dice results to determine both the answer as well
as whether there are unexpected events.  All from the same roll."
  name
  data
  (roller #'random-table/roller/default)
  (filter #'random-table/filter/default)
  (fetcher #'random-table/fetcher/default)
  (exclude-from-prompt nil)
  (private nil)
  (store nil)
  (reuse nil))

(cl-defun random-table/register (&rest kws &key name data exclude-from-prompt &allow-other-keys)
  "Store the DATA, NAME, and all given KWS in a `random-table'."
  ;; We need to guard for a reserved character; which we use for operations.
  (if (string-match-p "\\(\\[\\|\\]\\)" name)
      (user-error (concat "Attempt to register \"%s\" table failed.  "
			  "You cannot include the following characters:"
			  "\n\"[\", \"]\".")
		  name)
    (let* ((key (intern name))
           (struct (apply #'make-random-table
			  :name key
			  ;; When there's only one possible result, don't prompt the
			  ;; user when they chose the "I'll roll my own dice"
			  ;; option.
			  :exclude-from-prompt (or exclude-from-prompt
						   (= 1 (length (-list data))))
			  :data (-list data) kws)))
      (puthash key struct random-table/storage/tables))))

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
                    ((symbolp value)
                      (gethash value random-table/storage/tables))
                    ((stringp value)
                      (gethash (intern value) random-table/storage/tables))
                    ((integerp value)
                      nil)
                    (t
                      (error "Expected %s to be a `random-table', `symbol', `integer', or `string' got %s"
                        value
                        (type-of value))))))
    table
    (unless allow_nil
      (error "Could not find table %s; use `random-table/register'" value))))

(defvar random-table/storage/results
  (make-hash-table)
  "An ephemeral storage for various results of rolling.

As part of the rolling, we both add to and remove those stored
values; that is to say functions are responsible for clean-up.
See `random-table' for discussion about storage and reuse.")

(defvar random-table/storage/tables
  (make-hash-table)
  "A hash-table of random tables.

The hash key is the \"human readable\" name of the table (as a symbol).
The hash value is the contents of the table.")

;;; Random Table Roller
(cl-defmacro random-table/roller (&rest body &key label &allow-other-keys)
  "Create a LABEL named roller that \"rolls\" the BODY.

DEPRECATED!"
  ;; (message "⚠ WARNING: I have deprecated `random-table/roller'; See README.org for updated approach.")
  (let ((roller (intern (concat "random-table/roller/" label)))
         (docstring (format "Roll %s on given TABLE" label)))
    `(defun ,roller (table)
       ,docstring
       (if (and current-prefix-arg
             (not (random-table-exclude-from-prompt table)))
         (read-number (format "Roll %s for %s: "
                        ,label (random-table-name table)))
         ,@body))))

(defun random-table/roller/roll (table)
  "Roll given TABLE's registered :roller.

See `random-table'"
  (if-let ((roller (random-table-roller table)))
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
  "Given the TABLE roll randomly on it.
See `random-table/filter/default'.
See `random-table/roller' macro."
  ;; Constant off by one errors are likely
  (let ((faces (length (-list (random-table-data table)))))
    (if (and current-prefix-arg
          (not (random-table-exclude-from-prompt table)))
      (read-number (format "Roll 1d%s for %s: "
                     faces (random-table-name table)))
      (+ 1 (random faces)))))

(defun random-table/roller/string (text)
  "Interpolate given TEXT as roller."
  (if (string-match-p random-table/dice/regex text)
      (if current-prefix-arg
	  (read-number (format "Roll %s: " text))
	(string-to-number (format "%s" (random-table/dice/roll (s-trim text)))))
    (random-table/roll/parse-text text)))

(defun random-table/roller/seq (seq)
  "Interpolate given SEQ as roller."
  (let ((func (car seq))
        (rolls (mapcar
		(lambda (text)
                  (let ((value (if (random-table/prompt/get text)
				   (random-table/prompt text)
				 (random-table/roller/string text))))
                    (string-to-number (format "%s" value))))
                (cdr seq))))
    (apply func rolls)))

;; Perhaps not ideal to have one function per roll type.  But...having a
;; consistent interface.
;;
;; Notice these methods skip using the `random-table/dice/roll' method.  That
;; method, by design does not prompt the user for rolls.
(random-table/roller :label "1d2" (+ 1 (random 2)))
(random-table/roller :label "1d3" (+ 1 (random 3)))
(random-table/roller :label "1d4" (+ 1 (random 4)))
(random-table/roller :label "1d5" (+ 1 (random 5)))
(random-table/roller :label "1d6" (+ 1 (random 6)))
(random-table/roller :label "2d6" (+ 2 (random 6) (random 6)))
(random-table/roller :label "1d8" (+ 1 (random 8)))
(random-table/roller :label "1d10" (+ 1 (random 10)))
(random-table/roller :label "1d12" (+ 1 (random 12)))
(random-table/roller :label "1d20" (+ 1 (random 20)))
(random-table/roller :label "1d100" (+ 1 (random 100)))

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
    ;; Sniff out if the first element to see if we're dealing with a table that
    ;; has ranges.
    (if (-cons-pair? (car data))
      ;; We have a cons-pair, meaning we have multiple rolls mapping to the same
      ;; result.
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

(defcustom random-table/reporter
  #'random-table/reporter/as-kill-and-message
  "The function takes two positional parameters:

- EXPRESSION :: The text to evaluate for \"rolling\"
- RESULT :: The results of those rolls.

See `random-table/reporter/as-kill-and-message'."
  :group 'random-table
  :package-version '(random-table . "0.1.0")
  :type '(choice
          (function-item :tag "Kill and Message" random-table/reporter/as-kill-and-message)
          (function-item :tag "Insert" random-table/reporter/as-insert)))

(defun random-table/reporter/as-kill-and-message (expression results)
  "Report RESULTS of EXPRESSION as `message' and `kill'.

See `random-table/reporter'."
  (let ((text (format "%s :: %s" expression results)))
    (kill-new text)
    (message text)))

(defun random-table/reporter/as-insert (expression results &optional buffer)
  "Insert RESULTS of EXPRESSION into BUFFER.

See `random-table/reporter'."
  (with-current-buffer (or buffer (current-buffer))
    (end-of-line)
    (insert (format "\n%s :: %s\n" expression results))))

(defun random-table/roll/parse-text (text)
  "Roll the given TEXT.

Either by evaluating as a `random-table' or via `s-format'."
  (if-let* ((table (random-table/fetch text :allow_nil t)))
    (random-table/evaluate/table table)
    ;; We have specified a non-table; roll the text.  We'll treat a non-escaped
    ;; on as a dice text.
    (progn
      (let ((text (format "%s" text)))
        (s-format (if (string-match-p "\\${" text) text (format "${%s}" text))
          #'random-table/roll/parse-text/replacer)))))

(defvar random-table/roll/math-operation-regex
  "\\[\\(.*\\)\\][[:space:]]*\\(-\\|\\+\\|\\*\\)[[:space:]]*\\[\\(.*\\)\\]"

  "A regular expression with three capture regions:

- 1: left operand
- 2: operator (e.g. \"+\", \"-\", \"*\")
- 3. right operand")

(defvar random-table/roll/pass-roller-regex
  "\\([\\[+]\\)\\ \\[\\(.*\\)\\]"

  "A regular expression with two capture regions:

- 1: table name
- 2: roller expression (e.g. \"2d6\")")

(defvar random-table/current-roll
  nil)

(defun random-table/roll/parse-text/replacer (text &optional roller-expression)
  "Roll the TEXT; either from a table or as a dice-expression.

This is constructed as the replacer function of `s-format'.

WHen given ROLLER-EXPRESSION, use that instead of the table's roller."
  (if-let ((table (random-table/fetch text :allow_nil t)))
      (random-table/evaluate/table table roller-expression)
    (cond
     ((string-match random-table/roll/math-operation-regex text)
      (let* ((left-operand (match-string-no-properties 1 text))
             (operator (match-string-no-properties 2 text))
             (right-operand (match-string-no-properties 3 text)))
	;; TODO: Should we be passing the roller expression?
        (funcall (intern operator)
		 (string-to-number
		  (random-table/roll/parse-text/replacer left-operand))
		 (string-to-number
		  (random-table/roll/parse-text/replacer right-operand)))))
     ((string-match random-table/roll/pass-roller-regex text)
      (let* ((table-name (match-string-no-properties 1 text))
	     (roller-expression (match-string-no-properties 2 text)))
	(funcall (random-table/roll/parse-text/replacer
		  table-name
		  roller-expression))))
     ((and random-table/current-roll
	   (string-match "current_roll" text))
      random-table/current-roll)
     (t
      ;; Ensure that we have a dice expression
      (if (string-match-p random-table/dice/regex (s-trim text))
          (format "%s" (random-table/dice/roll (s-trim text)))
        text)))))

(defun random-table/evaluate/table (table &optional roller-expression)
  "Evaluate the random TABLE, optionally using the given ROLLER-EXPRESSION.

See `random-table' structure."
  (let* ((data (random-table-data table))
          (name (random-table-name table))
          (rolled (random-table/evaluate/table/roll table roller-expression)))
    ;; TODO: This is wildly naive.  Perhaps the current_roll needs to be
    ;; replaced with the "${Current Roll for [My Tablename]}".  Then we can
    ;; Cache that rolled value and retrieve it.
    (setq random-table/current-roll rolled)
    (let* ((filtered (apply (random-table-filter table) (-list rolled)))
            (row (if filtered
                   (funcall (random-table-fetcher table) data (-list filtered))
                   nil))
            (results (or (when row (random-table/roll/parse-text row)) "")))
      (setq random-table/current-roll nil)
      results)))

(defun random-table/evaluate/table/roll (table &optional roller-expression)
  "Roll on the TABLE, conditionally using ROLLER-EXPRESSION

This function favors re-using and caching values.

Why cache values?  Some tables you roll one set of dice and then
use those dice to lookup on other tables."
  (let ((results
         (or (when-let ((reuse-table-name (random-table-reuse table)))
               (or
		(random-table/storage/results/get reuse-table-name)
                (random-table/evaluate/table/roll-table
		 (random-table/fetch reuse-table-name) roller-expression)))
             (random-table/evaluate/table/roll-table table roller-expression))))
    (when (random-table-store table)
      (random-table/storage/results/put (random-table-name table) results))
    results))

(defun random-table/storage/results/get (name)
  (gethash (if (symbolp name) name (intern name))
	   random-table/storage/results))

(defun random-table/storage/results/put (name value)
  (puthash (if (symbolp name) name (intern name))
	   value
	   random-table/storage/results))

(defun random-table/evaluate/table/roll-table (table &optional roller-expression)
  "Roll on the TABLE optionally using the given ROLLER-EXPRESSION.

Or fallback to TABLE's roller slot."
  (if roller-expression
      (user-error "Cannot yet handle roller-expression")
    (random-table/roller/roll table)))

;;; Dice String Evaluator
;;
;; The following code (with the function name prefix of \"random-table/dice\"
;; is derived from Pelle Nilsson's decide.el package
;;
;; TODO Consider preserving the granular results (e.g. die one rolled a 4, etc)
(defun random-table/dice/roll (spec-string)
  "Evaluate the given SPEC-STRING by parsing as a dice expression."
  (apply #'random-table/dice/roll-spec
    (random-table/dice/parse-spec spec-string)))

(defvar random-table/dice/regex
  "^\\([1-9][0-9]*\\)d\\([0-9]*\\)\\([+-][0-9]*\\)?")

(defun random-table/dice/parse-spec (spec)
  "Convert SPEC to list:

   - Number of dice
   - Face
   - Adder

  e.g. \"1d6\" -> (1 6 0) or \"2d10+2\" -> (2 10 2)"
  (if (string= "d66" spec)
      (+ (* 10 (+ 1 (random 6))) (+ 1 (random 6)))
    (when (string-match
           random-table/dice/regex
           spec)
      (list (random-table/dice/string-to-number
	     (match-string 1 spec) 1)
	    (random-table/dice/string-to-number
	     (match-string 2 spec) 6)
	    (random-table/dice/string-to-number
	     (match-string 3 spec) 0)))))

(defun random-table/dice/string-to-number (spec default)
  "Convert the SPEC (and DEFAULT) into an integer."
  (let ((n (if (stringp spec)
             (string-to-number spec)
             0)))
    (cond ((null spec) default)
      ((> n 0) n)
      ((string= "+" spec) 0)
      ((string= "-" spec) 0)
      (t spec))))

(defun random-table/dice/roll-spec (number-dice faces modifier)
  "Roll the NUMBER-DICE each with FACES number of sides and add MODIFIER."
  ;; TODO Consider returning a list for further inspection.
  (let ((amount modifier))
    (dotimes (i number-dice)
      (setq amount (+ amount 1 (random faces))))
    amount))

(defvar random-table/prompt/registry
  (make-hash-table)
  "Stores the prompts registered by `random-table/prompt/register'.")

(defun random-table/completing-read-alist (prompt alist &rest args)
  "Like `completing-read' but PROMPT to find value in given ALIST.

ARGS are passed to `completing-read'."
  (alist-get (apply #'completing-read prompt alist args)
	     alist nil nil #'string=))

(defun random-table/completing-read-integer-range (prompt range)
  "Like `completing-read' but PROMPT to find integer value in RANGE."
  (let ((strings (mapcar #'number-to-string range)))
    (string-to-number (completing-read prompt strings nil t))))

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
		   `(random-table/completing-read-integer-range
		     ,prompt ,range))
		  ((eq type #'read-number)
		   `(read-number ,prompt ,default))
		  ((eq type #'completing-read)
		   `(random-table/completing-read-alist
		     ,prompt ,range nil t))
		  (t (user-error
		      "Unknown type %s function for %s registry"
		      type name)))))
    (let ((value (or (random-table/storage/results/get name)
                     (apply (random-table/prompt/get name)))))
      (random-table/storage/results/put name value)
      value)))

(defun random-table/prompt/get (name)
  (gethash (intern name) random-table/prompt/registry))

(defun random-table/prompt/put (name value)
  (puthash (intern name) value random-table/prompt/registry))

(defun random-table/roll/test-all ()
  "A convenience function to test all of the public `random-table' entries."
  (maphash (lambda (key table)
	     (unless (random-table-private table)
	       (message "Testing %s table" key)
	       ;; The test does not call these interactively, but the methods
	       ;; assume a current-prefix-arg
	       (funcall #'random-table/roll (random-table-name table))))
	   random-table/storage/tables))

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
                       ;; Predicate that filters out non-private tables.
                       (lambda (name table &rest args)
                         (not (random-table-private table))))))
  (setq random-table/storage/results (make-hash-table))
  ;; TODO: Consider allowing custom reporter as a function.  We already
  ;; register it in the general case.
  (funcall random-table/reporter
    text
    (random-table/roll/parse-text text))
  (setq random-table/storage/results nil))

(provide 'random-table)
;;; random-table.el ends here
