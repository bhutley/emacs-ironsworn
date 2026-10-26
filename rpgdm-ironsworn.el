;;; rpgdm-ironsworn -- Functions for integrating Ironsworn with Org  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: September 18, 2020
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;    This file is the source of truth for the code.  It was
;;    originally tangled from README.org, which is now documentation
;;    only and no longer tangles.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'org)
(require 's)
(require 'seq)
(require 'rpgdm)

(defvar rpgdm-ironsworn-project (file-name-directory load-file-name)
  "The root directory to the rpgdm-ironsworn project.")

(defun rpgdm-ironsworn--results (action modifier one-challenge two-challenge
                                 &optional momentum)
  "Return formatted string for an Ironsworn dice roll results.
The ACTION is the d6 which is added to the MODIFIER (which can
have character attribute values as well as any bonuses.  The sum
is compared to the two d10, ONE-CHALLENGE and TWO-CHALLENGE.

The optional MOMENTUM can be specified to add a message that the
use could burn that in order to improve the roll."
  (unless momentum
    (setq momentum 0))

  (cl-flet ((strong-p (value dice1 dice2) (and (> value dice1) (> value dice2)))
            (weak-p   (value dice1 dice2)  (or (> value dice1) (> value dice2)))
            (miss-p   (value dice1 dice2) (and (<= value dice1) (<= value dice2)))
            (faded    (str) (propertize str 'face '(:foreground "#888")))
            (noted    (str) (propertize str 'face '(:foreground "light blue")))
            (strong   (str) (propertize str 'face '(:foreground "green")))
            (weak     (str) (propertize str 'face '(:foreground "yellow")))
            (interest (str) (propertize str 'face '(:foreground "orange")))
            (miss     (str) (propertize str 'face '(:foreground "red"))))

    (let* ((action-results (+ action modifier))
           (str-results (cond
                         ((strong-p action-results one-challenge two-challenge)
                          (strong "Strong hit"))
                         ((weak-p action-results one-challenge two-challenge)
                          (weak "Weak hit"))
                         (t (miss "Miss"))))
           (burn-msg     (if (> momentum action-results)
                             (cond
                              ((and (strong-p momentum one-challenge two-challenge)
                                    (not (strong-p action-results one-challenge two-challenge)))
                               (concat " -- Burn momentum for a " (strong "Strong hit")))
                              ((and (weak-p momentum one-challenge two-challenge)
                                    (miss-p action-results one-challenge two-challenge))
                               (concat " -- Burn momentum for a " (weak "Weak hit")))
                              (t ""))
                           ""))
           (matched-msg  (if (= one-challenge two-challenge)
                             (concat " ← " (interest "Create a Twist"))
                           "")))

      (format "%s %s %d %s%d %s %d%s %s %d %s %d%s%s"
              str-results         (faded "::")
              (+ action modifier) (faded "(")
              action              (faded "+")
              modifier            (faded ")")
              (noted "→")
              one-challenge       (faded "/")
              two-challenge
              matched-msg burn-msg))))

(defun rpgdm-ironsworn-roll (modifier &optional momentum)
  "Display a Hit/Miss message based on an Ironsworn roll.
Done by rolling and comparing a d6 action roll (summed with
MODIFIER) vs two d10 challenge dice.  If given, the MOMENTUM may
trigger a message to the user that they can burn that for better
results."
  (interactive "nModifier: ")
  (let ((one-challenge (rpgdm--roll-die 10))
	(two-challenge (rpgdm--roll-die 10))
	(action-roll   (rpgdm--roll-die 6)))
    (rpgdm-message (rpgdm-ironsworn--results action-roll modifier
					     one-challenge two-challenge
					     momentum))))

(defun rpgdm-ironsworn--new-character-template (&optional name)
  "Insert basic Ironsworn template at the end of the current buffer.
A header is created with NAME, but if this is an empty string,
a random name is generated for the purposes of the template."
  (when (or (null name) (s-blank? name))
    (setq name (rpgdm-tables-choose "name/ironlander")))

  (let ((frmt (seq-random-elt '("* The Adventures of %s"
                                "* The Journeys of %s"
                                "* %s, an Epic Saga"
                                "* The Epic of %s"
                                "* Travels of %s"))))
    (goto-char (point-max))
    ;; (insert "# Local Variables: # eval: (progn (require 'rpgdm-ironsworn) (rpgdm-mode)) # End: ")
    (insert (format frmt name))
    (insert "

")))

(defun rpgdm-ironsworn--character-asset-label (filename)
  "Given a FILENAME of an Ironsworn asset, return an Asset label."
  (cl-flet* ((convert (str) (s-replace "-" " " str))
             (uppity   (str) (s-titleize (convert str))))
    (when (string-match (rx (one-or-more any) "/"
                            ;; parent directory
                            (group (one-or-more (not "/"))) "/"
                            ;; base filename
                            (group (one-or-more (not "/"))) ".org")
                        filename)
      (format "%s :: %s"
              (uppity  (match-string 1 filename))
              (convert (match-string 2 filename))))))     ; Keep lowercase for ease of access

(defvar rpgdm-ironsworn-character-assets nil
  "Association list of descriptive label and the filename of each Ironsworn asset.")

(defun rpgdm-ironsworn-character-assets ()
  "Return an association list of all available assets.
The `car' is a label for the asset, and the `cdr' is the filename
that contains the text.  The first time we call this, we read from
the `assets' directory, otherwise, we return a cached version."
  (unless rpgdm-ironsworn-character-assets
    (let ((asset-files (thread-first rpgdm-ironsworn-project
                                     (f-join "assets")
                                     (directory-files-recursively (rx (one-or-more any) ".org") nil))))

      (setq rpgdm-ironsworn-character-assets
            (seq-map (lambda (file) (cons (rpgdm-ironsworn--character-asset-label file) file))
                     asset-files))))

  rpgdm-ironsworn-character-assets)

(defun rpgdm-ironsworn--pick-character-asset ()
  "Completing read for an Ironsworn asset."
  (let ((choice (completing-read "Which asset: " (rpgdm-ironsworn-character-assets))))
    (thread-first choice
      (assoc rpgdm-ironsworn-character-assets 'equal)
      (cdr))))

(defvar rpgdm-ironsworn-new-character nil "Are we in the process of creating a new character?")

(defun rpgdm-ironsworn-insert-character-asset (asset)
  "Choose and insert the contents of an ASSET in the current buffer."
  (interactive (list (rpgdm-ironsworn--pick-character-asset)))
  (when rpgdm-ironsworn-new-character (goto-char (point-max)))
  (let ((file (if (consp asset) (cdr asset) asset)))
    (ignore-errors
      (insert-file-contents file nil))))

(defun rpgdm-ironsworn-show-character-asset (asset)
  "Choose and insert the contents of an ASSET in the current buffer."
  (interactive (list (rpgdm-ironsworn--pick-character-asset)))
  (let ((asset-file (if (consp asset) (cdr asset) asset))
        (orig-buf (window-buffer)))
    (ignore-errors
      (find-file-other-window asset-file)
      (goto-char (point-min))
      (pop-to-buffer orig-buf))))

(defun rpgdm-ironsworn--good-character-assets (assets)
  "Return ASSET-FILES if all given are _good enough_.
  That is, all are unique, only one companion, etc."
  (cl-flet ((only-files (entry) (if (consp entry) (cdr entry) entry))
            (is-companion? (file) (string-match (rx "companions") file))
            (not-at-first? (file) (when (or (s-ends-with? "revenant.org" file)
                                            (s-ends-with? "weaponmaster.org" file)
                                            (s-ends-with? "masked.org" file)
                                            (s-ends-with? "battle-scarred.org" file)
                                            (s-ends-with? "ritualist.org" file)
                                            (s-ends-with? "shadow-kin.org" file)
                                            (s-ends-with? "oathbreaker.org" file))
                                    t)))
    (let* ((asset-files (-map #'only-files assets))
           (num-of-companions (seq-count #'is-companion? asset-files)))
      (when (and
             ;; Are all the assets in the list unique?
             (equal asset-files (seq-uniq asset-files))
             ;; Does the list only include first-time-only?
             (-none? #'not-at-first? asset-files)
             ;; Does the list include, at most, one companion?
             (<= num-of-companions 1))
        assets))))

(defun rpgdm-ironsworn--some-character-assets (asset-filenames &optional number)
  "Return a list of NUMBER elements from ASSET-FILENAMES... randomly.
If NUMBER is nil, then return 3."
  (unless number
    (setq number 3))
  (cl-loop for x from 1 to number
        collect (seq-random-elt asset-filenames)))

(defun rpgdm-ironsworn--random-character-assets (&optional number-of-assets)
  "Return the file names of NUMBER-OF-ASSETS from the `assets' directory.
The chosen assets are _good_ in that they won't have duplicates, etc."
  (let ((assets (rpgdm-ironsworn-character-assets)))
    (cl-loop for answer = (thread-first assets
                            (rpgdm-ironsworn--some-character-assets number-of-assets)
                            (rpgdm-ironsworn--good-character-assets))
             until answer
             finally return answer)))

(defun rpgdm-ironsworn-random-character-assets (&optional number-of-assets)
  "Return the file names of NUMBER-OF-ASSETS from the `assets' directory.
The chosen assets are _good_ in that they won't have duplicates, etc."
  (interactive "nHow many random assets should we insert? ")
  (dolist (file (rpgdm-ironsworn--random-character-assets number-of-assets))
    (rpgdm-ironsworn-insert-character-asset file)))

(defun rpgdm-ironsworn-insert-character-assets ()
  (ignore-errors
    (call-interactively 'rpgdm-ironsworn-insert-character-asset))
  (when (y-or-n-p "Insert another asset? ")
    (rpgdm-ironsworn-insert-character-assets)))

(defun rpgdm-ironsworn--new-character-assets ()
  "Insert the contents of three character assets from the assets directory."
  (goto-char (point-max))
  (insert "\n** Assets\n")
  (if (y-or-n-p "Would you like three random assets? ")
      (rpgdm-ironsworn-random-character-assets 3)
    (if (y-or-n-p "Would you like to choose your assets? ")
        (rpgdm-ironsworn-insert-character-assets))))

(defun rpgdm-ironsworn--new-character-stats ()
  "Insert character stats after querying user for them.
Note: The stats are added as properties using the
`rpgdm-ironsworn-store-character-state' and the
`rpgdm-ironsworn-progress-create' functions."
  (dolist (stat '(edge heart iron shadow wits))
    (rpgdm-ironsworn-store-character-state stat
        (read-string (format "What '%s' stat: " stat))))

  (dolist (stat '(health spirit supply))
    (rpgdm-ironsworn-store-character-state stat 5))
  (rpgdm-ironsworn-store-character-state 'momentum 2)

  (rpgdm-ironsworn-progress-create (read-string "What title should we give this new character's Epic vow: ") 1)
  (rpgdm-ironsworn-progress-create "Bonds" 1)
  (rpgdm-ironsworn-progress-mark "Bonds")

  (rpgdm-ironsworn--top-heading)
  (re-search-forward (rx line-start (zero-or-more space) line-end))
  (insert "\n** Bonds\n")
  (insert (format "  - My home settlement of %s\n" (rpgdm-tables-choose "settlement/name"))))

(defun rpgdm-ironsworn--new-character-stats-first ()
  "Insert a new character's details at the end of the buffer.
The character stats are first queried, and then assets inserted."
  (goto-char (point-max))
  (rpgdm-ironsworn--new-character-stats)
  (rpgdm-ironsworn--new-character-assets))

(defun rpgdm-ironsworn--new-character-assets-first ()
  "Insert a new character's details at point.
The assets are inserted first, and then character stats are queried."
  ;; Saving and restoring point, means the properties should be in the
  ;; correct, top-level position.
  (let ((p (point)))
    (rpgdm-ironsworn--new-character-assets)
    (goto-char p)
    (rpgdm-ironsworn--new-character-stats)))

(defun rpgdm-ironsworn-new-character (name order)
  "Interactively query the user for a new character's attribute.
The NAME is the character's name, and ORDER determines how the
template will generate and query the user for the rest of the data.
This function _appends_ this information to the current buffer,
which should be using the `org-mode' major mode."
  (interactive (list
                (read-string "What is the new character's name? ")
                (completing-read "What order should we build this? " '("Statistics first" "Assets first"))))

  (setq rpgdm-ironsworn-new-character t)
  (rpgdm-ironsworn--new-character-template name)
  (if (equal order "Assets first")
      (rpgdm-ironsworn--new-character-assets-first)
    (rpgdm-ironsworn--new-character-stats-first))
  (setq rpgdm-ironsworn-new-character nil)
  (message "Alright, the template for %s is complete. Edit away!" name))

(defun rpgdm-ironsworn--display-stat (stat character)
  "Colorized the STAT from a CHARACTER hash containing it.
See `rpgdm-ironsworn-character-display'."
  (let ((value (gethash stat character))
        s-val color)
    (if value
        (setq s-val (number-to-string value)
              color (cond
                     ((< value 1) "red")
                     ((< value 3) "orange")
                     ((< value 4) "yellow")
                     (t "green")))
      (setq s-val "??" color "purple"))
    (propertize s-val 'face `(:foreground ,color))))

(defun rpgdm-ironsworn-character-display ()
  "Easily display the character's stats and other things."
  (interactive)
  (let ((character (rpgdm-ironsworn-current-character-state)))
    (rpgdm-message "Edge: %d  Heart: %d  Iron: %d  Shadow: %d  Wits: %d
Health: %s  Spirit: %s  Supply: %s  Momentum: %d  %s"
                   (rpgdm-ironsworn-character-stat 'edge character)
                   (rpgdm-ironsworn-character-stat 'heart character)
                   (rpgdm-ironsworn-character-stat 'iron character)
                   (rpgdm-ironsworn-character-stat 'shadow character)
                   (rpgdm-ironsworn-character-stat 'wits character)

                   (rpgdm-ironsworn--display-stat 'health character)
                   (rpgdm-ironsworn--display-stat 'spirit character)
                   (rpgdm-ironsworn--display-stat 'supply character)

                   (gethash 'momentum character 5)
                   (rpgdm-ironsworn--asset-stat-show-all character))))

(defun rpgdm-ironsworn-to-string (a)
  "Return a lowercase string from either A, a string, keyword or symbol."
  (downcase
   (cond
    ((keywordp a) (substring (symbol-name a) 1))
    ((symbolp  a) (symbol-name a))
    (t            a))))

(define-hash-table-test 'str-or-keys
  (lambda (a b)
    (string-equal (rpgdm-ironsworn-to-string a)
                  (rpgdm-ironsworn-to-string b)))

  (lambda (s) (sxhash-equal (rpgdm-ironsworn-to-string s))))

(defun rpgdm-ironsworn-character-stat (stat &optional character)
  "Return integer value associated with a character's STAT.
If CHARACTER doesn't refer to a character hash, then this calls
the `rpgdm-ironsworn-current-character-state' function."
  (unless character
    (setq character (rpgdm-ironsworn-current-character-state)))
  (gethash stat character 0))

(defun rpgdm-ironsworn--read-stat (label)
  "A `read-string', but for the changeable value associated with LABEL.
A `+1' means increasing the stat by 1, while a `-1' decreased that stat.
A `=3' sets the stat to the number (in this case, `3'), and `=-2'
sets it to a negative number.

Hitting return (entering a blank value) increments the `momentum' stat,
but decrements any other stats by `1'.  Any other value means to take
the default for that stat."
  (let ((value (read-string (format "Adjustment to %s (+/-/= for absolute value): " label)))
	(rxnum (rx (group (optional (or "+" "-" "="))) (* space) (group (optional "-") (+ digit)) (* space))))

    (if (string-match rxnum value)
	(let ((sign                   (match-string 1 value))
	      (numb (string-to-number (match-string 2 value))))
	  (cond
	   ((equal sign "-") `(:decrease ,numb))
	   ((equal sign "+") `(:increase ,numb))
	   ((equal sign "=") `(:absolute ,numb))
	   (t                (if (eq label `momentum) `(:increase ,numb) `(:decrease ,numb)))))

      (if (string-blank-p value)
	  (if (eq label 'momentum) '(:increase 1) '(:decrease 1))
	'(:reset 0)))))

(defvar rpgdm-ironsworn-stat-limits '((health    0 5)
                                       (spirit    0 5)
                                       (supply    0 5)
                                       (momentum -6 10))
  "Alist of stat and its minimum and maximum values.
Stats not listed here, like asset stats, are not limited.")

(defun rpgdm-ironsworn--clamp-stat (stat value)
  "Return VALUE limited to the range of STAT in `rpgdm-ironsworn-stat-limits'."
  (if-let ((limits (alist-get stat rpgdm-ironsworn-stat-limits)))
      (max (car limits) (min (cadr limits) value))
    value))

(defun rpgdm-ironsworn-adjust-stat (stat &optional default)
  "Increase or decrease the current character's STAT by ADJ.
If the STAT isn't found, returns DEFAULT.  If there is no DEFAULT,
the stat is left unchanged."
  (let* ((tuple (rpgdm-ironsworn--read-stat stat))
	 (curr  (rpgdm-ironsworn-character-stat stat))
	 (oper  (car tuple))
	 (numb  (cadr tuple))
	 (new   (cl-case oper
		  (:increase (+ curr numb))
		  (:decrease (- curr numb))
		  (:absolute numb)
		  (t         default))))
    ;; (message "Combining curr %d with %d with %s operator" curr numb oper)
    (when new
      (rpgdm-ironsworn-store-character-state stat (rpgdm-ironsworn--clamp-stat stat new)))))

(defun rpgdm-ironsworn-adjust-health ()
  "Increase or decrease the current character's health interactively."
  (interactive)
  (rpgdm-ironsworn-adjust-stat 'health 5))

(defun rpgdm-ironsworn-adjust-spirit ()
  "Increase or decrease the current character's spirit interactively."
  (interactive)
  (rpgdm-ironsworn-adjust-stat 'spirit 5))

(defun rpgdm-ironsworn-adjust-supply ()
  "Increase or decrease the current character's supply interactively."
  (interactive)
  (rpgdm-ironsworn-adjust-stat 'supply 5))

(defun rpgdm-ironsworn-adjust-momentum ()
  "Increase or decrease the current character's momentum interactively."
  (interactive)
  (rpgdm-ironsworn-adjust-stat 'momentum 2))

(defun rpgdm-ironsworn-roll-stat (stat modifier)
  "Roll an action based on a loaded character's STAT with a MODIFIER."
  (interactive (list (completing-read "Stat Modifier: " '(Edge Heart Iron Shadow Wits))
                     (read-string "Other Modifier: ")))
  (let ((all-mods (+ (rpgdm-ironsworn-character-stat stat)
                     (string-to-number modifier)))
        (momentum (rpgdm-ironsworn-character-stat :momentum)))
    (rpgdm-ironsworn-roll all-mods momentum)))

(defun rpgdm-ironsworn-roll-edge (modifier)
  "Roll an action based on a loaded character's Edge stat with a MODIFIER."
  (interactive (list (read-string "Edge + Modifier: ")))
  (rpgdm-ironsworn-roll-stat :edge modifier))

(defun rpgdm-ironsworn-roll-heart (modifier)
  "Roll an action based on a loaded character's Heart stat with a MODIFIER."
  (interactive (list (read-string "Heart + Modifier: ")))
  (rpgdm-ironsworn-roll-stat :heart modifier))

(defun rpgdm-ironsworn-roll-iron (modifier)
  "Roll an action based on a loaded character's Iron stat with a MODIFIER."
  (interactive (list (read-string "Iron + Modifier: ")))
  (rpgdm-ironsworn-roll-stat :iron modifier))

(defun rpgdm-ironsworn-roll-shadow (modifier)
  "Roll an action based on a loaded character's Shadow stat with a MODIFIER."
  (interactive (list (read-string "Shadow + Modifier: ")))
  (rpgdm-ironsworn-roll-stat :shadow modifier))

(defun rpgdm-ironsworn-roll-wits (modifier)
  "Roll an action based on a loaded character's Wits stat with a MODIFIER."
  (interactive (list (read-string "Wits + Modifier: ")))
  (rpgdm-ironsworn-roll-stat :wits modifier))

(defun rpgdm-ironsworn-roll-health ()
  "Roll challenge dice and compare with character's current health."
  (interactive)
  (rpgdm-ironsworn-progress-roll
   (rpgdm-ironsworn-character-stat :health)))

(defun rpgdm-ironsworn-roll-spirit ()
  "Roll challenge dice and compare with character's current spirit."
  (interactive)
  (rpgdm-ironsworn-progress-roll
   (rpgdm-ironsworn-character-stat :spirit)))

(defun rpgdm-ironsworn-roll-supply ()
  "Roll challenge dice and compare with character's current supply."
  (interactive)
  (rpgdm-ironsworn-progress-roll
   (rpgdm-ironsworn-character-stat :supply)))

(defun rpgdm-ironsworn--asset-stat-key (name)
  "Convert a string, NAME, into an `asset-str' symbol."
  (thread-last name
    (s-replace-regexp (rx space) "-")
    (s-replace-regexp (rx (one-or-more (not (any "-" alphanumeric)))) "")
    (downcase)
    (format "asset-%s")
    (make-symbol)))

(defun rpgdm-ironsworn--asset-stat-name (stat)
  "Convert an asset-related STAT symbol into a readable name."
  (let ((no-asset (thread-first stat
                    (symbol-name)
                    (substring 6))))
    (thread-last no-asset
      (string-replace "-" " ")
      (s-titleize))))

(defun rpgdm-ironsworn-asset-stat-create (name value)
  "Create a special stat associated with an asset.
Note that this is created as an org property value at the
top-most heading. The NAME is a short string for the name of the
asset, and VALUE is the initial value, probably an integer
value."
  (interactive (list (read-string "Asset stat name: ")
                     (read-string "Initial asset stat value: ")))
  (unless (s-blank? name)
    (let ((stat (rpgdm-ironsworn--asset-stat-key name)))
      (rpgdm-ironsworn-store-default-character-state stat value))))

(defun rpgdm-ironsworn--asset-stat-alist (&optional stats)
  "Return alist of all defined asset-related STATS."
  (unless stats
    (setq stats (rpgdm-ironsworn-current-character-state)))
  (let* ((keys  (thread-last stats
                  (hash-table-keys)
                  (--filter (string-prefix-p "asset-" (symbol-name it)))))
         (names (-map 'rpgdm-ironsworn--asset-stat-name keys)))
    (-zip-pair names keys)))

(defun rpgdm-ironsworn--asset-stat-choose ()
  "Display all asset-related stats, allowing user to choose one.
Return the symbol, `asset-xyz."
  (let* ((choices (rpgdm-ironsworn--asset-stat-alist))
         (choice  (completing-read "Choose Asset Stat: " choices)))
    (alist-get choice choices 0 nil 'equal)))

(defun rpgdm-ironsworn-asset-stat-adjust (stat)
  "Adjust an asset-related STAT, after choosing that.
Note that the stat must have already been defined."
  (interactive (list (rpgdm-ironsworn--asset-stat-choose)))
  (rpgdm-ironsworn-adjust-stat stat))

(defun rpgdm-ironsworn-asset-stat-show (stat)
  "Display an asset STAT after selecting it from those created."
  (interactive (list (rpgdm-ironsworn--asset-stat-choose)))
  (message "%d" (rpgdm-ironsworn-character-stat stat)))

(defun rpgdm-ironsworn--asset-stat-show-all (&optional stats)
  "Return string showing all asset-related STATS and their values.
Note that if STATS is nil, the stats are acquired by calling
`rpgdm-ironsworn-current-character-state'."
  (unless stats
    (setq stats (rpgdm-ironsworn-current-character-state)))

  (cl-flet ((convert (stat) (cons (car stat)
                                  (rpgdm-ironsworn-character-stat (cdr stat)))))
    (thread-last stats
      (rpgdm-ironsworn--asset-stat-alist)
      (-map #'convert)
      (--map (format "%s: %s" (car it) (cdr it)))
      (s-join "  "))))

(defun rpgdm-ironsworn-asset-stat-roll (stat modifier)
  "Use an asset-related stat as a modifier for a standard roll."
  (interactive (list (rpgdm-ironsworn--asset-stat-choose)
                     (read-string "Other modifier: ")))
  (rpgdm-ironsworn-roll-stat stat modifier))

(defun rpgdm-ironsworn--move-tuple (file)
  "Return a list of a string representation of FILE, and FILE.
The string representation is created by looking at the parent
directory and file name."
  (let* ((regx (rx "moves/"
		   (group (one-or-more (not "/")))
		   "/"
		   (group (one-or-more (not ".")))
		   ".org" eol))
	 (_    (string-match regx file))
	 (type (thread-last file
		 (match-string 1)
		 (s-titleize)))
	 (name (thread-last file
		 (match-string 2)
		 (s-replace-regexp "-" " "))))
    (list (format "%s :: %s" type name) file)))

(defvar rpgdm-ironsworn-moves ()
  "A list of tuples of the move and the file containing its goodness.")

(defun rpgdm-ironsworn-moves ()
  "Return a list containing available moves and its filename.
The file contains the move's instructions and other properties.  Note
that this function is memoized, in that re-calling this function
will return a cached copy."
  (unless rpgdm-ironsworn-moves
    (setq rpgdm-ironsworn-moves
	  (mapcar 'rpgdm-ironsworn--move-tuple
		  (directory-files-recursively
		   (f-join rpgdm-ironsworn-project "moves")
                   (rx (1+ any) ".org" eos)))))
  rpgdm-ironsworn-moves)

(defun rpgdm-ironsworn--completing-read-value (prompt values)
  "Like `completing-read' but return value from VALUES instead of key.
Display PROMPT, and has a list of choices displayed for the user to select."
  (thread-first prompt
    (completing-read values)
    (assoc values)
    (cadr)))

(defun rpgdm-ironsworn-choose-move ()
  "A `completing-read' for moves, but returns the move filename."
  (rpgdm-ironsworn--completing-read-value "Move: " (rpgdm-ironsworn-moves)))

(defun rpgdm-ironsworn--store-move (title results)
  "Store RESULTS in `m' register for later pasting.
The register also has TITLE, the name of the move, based on the
current file."
  (set-register ?m (format "# %s ... %s " title results)))

(defun rpgdm-ironsworn-make-move (move-file)
  "Make an Ironsworn move by loading MOVE-FILE.
Optionally query the user to make an initial roll based
on the properties in the file."
  (interactive (list (rpgdm-ironsworn-choose-move)))

  ;; Normally, we'd call `save-window-excursion', however, that buries the file
  ;; we show, and I think we should leave it up for study.
  (let (props title
	(orig-buf (window-buffer)))
    (find-file-other-window move-file)
    (goto-char (point-min))
    (setq title (cdr (assoc "ITEM" (org-entry-properties))))
    (setq props (car (org-property-values "move-stats")))
    (pop-to-buffer orig-buf)
    (rpgdm-ironsworn--store-move title (or (rpgdm-ironsworn--make-move props) ""))))

(defun rpgdm-ironsworn--make-move (move-props)
  "Query user for rolls based on one or series of MOVE-PROPS.
Moves without MOVE-PROPS, like Pay the Price, don't roll anything."
  (if (and move-props
           (string-match (rx (group (1+ alpha)) "|" (group (1+ any))) move-props))
      (let ((first (match-string 1 move-props))
            (rest  (match-string 2 move-props)))
        (rpgdm-ironsworn--make-single-move first)
        (rpgdm-ironsworn--make-move        rest))
    (rpgdm-ironsworn--make-single-move move-props)))

(defun rpgdm-ironsworn--make-single-move (move-props)
  "Query user for rolls based on one MOVE-PROPS."
  (let* ((props (s-split " " (or move-props "")))
         (count (seq-length props))
         (stats (seq-filter (lambda (s) (string-match (rx (one-or-more alpha)) s)) props))
         (first (car props)))
    (cond
     ((seq-empty-p stats)       nil)
     ((equal first "track")     (call-interactively 'rpgdm-ironsworn-progress-create))
     ((equal first "progress")  (call-interactively 'rpgdm-ironsworn-progress-roll))
     ((= count 1)               (rpgdm-ironsworn-roll-with first))
     ((equal first ">")         (rpgdm-ironsworn-roll-best stats))
     (t                         (rpgdm-ironsworn-roll-stat
                                 (completing-read "Stat Choice: " stats)
                                 (read-string "Modifier: "))))))

(defun rpgdm-ironsworn-roll-with (stat)
  "Roll against a character STAT, as a string, and prompt for modifier.
See `rpgdm-ironsworn-roll-stat' for details."
  (rpgdm-ironsworn-roll-stat stat (read-string "Modifier: ")))

(defun rpgdm-ironsworn-roll-best (options)
  "Call `rpgdm-ironsworn-roll' with a characters largest stat from OPTIONS."
  (let* ((values  (seq-map 'rpgdm-ironsworn-character-stat options))
         (largest (seq-max values))
         (modifier (read-string "Modifier: "))
         (all-mods (+ largest (string-to-number modifier))))
    (rpgdm-ironsworn-roll all-mods)))

(defvar rpgdm-ironsworn-progress-levels
  (cl-flet* ((faded (str) (propertize str 'face '(:foreground "#888")))
             (msg   (a b) (format "%s %s %s" a (faded "--") (faded b))))
    `((,(msg "troublesome" "quick") 12)
      (,(msg "dangerous" "short") 8)
      (,(msg "formidable" "long") 4)
      (,(msg "extreme" "very long") 2)
      (,(msg "epic" "never-ending") 1)))
  "The five levels of progression for an Ironsworn progress track.")

(defun rpgdm-ironsworn-progress-level (label)
  "Return the level (number of ticks to mark) of progress LABEL.
For instance, if LABEL is `Dangerous', this returns `8'."
  (alist-get label rpgdm-ironsworn-progress-levels 8 nil 'equal))

(defun rpgdm-ironsworn-progress-level-label (level)
  "Return the label associated with the progress LEVEL, e.g. dangerous."
  (thread-last rpgdm-ironsworn-progress-levels
               (--filter (= (cadr it) level))
               (car)      ; Assume we only get one match
               (car)      ; Get the textual label
               (s-split " ")
               (car)))    ; Word before the space

(defun rpgdm-ironsworn-progress-track-choose (&optional allow-other)
  "Query the user to choose a track stored in the org file.
If ALLOW-OTHER is non-nil, we append a choice, <other>, which
allows the user to choose the number of squares that have been
marked against some progress."
  (let* ((other "<other>")
	 (tracks  (rpgdm-ironsworn-character-progresses))
	 (choices (if allow-other
		      (append tracks (list other))
		    tracks))
	 (original (completing-read "Progress Track: " choices)))
    (if (and allow-other (equal original other))
	(read-number "Completed Track Amount [0-10]: ")
      original)))

(defun rpgdm-ironsworn-progress-create (name level)
  "Add a new progress track, NAME, of a particular LEVEL.
Stored as a property in the org file.  Keep in mind that the
NAME should be a short title, not a description."
  (interactive (list (read-string "Progress Name: ")
                     (rpgdm-ironsworn--completing-read-value "Progress Level: "
                                            rpgdm-ironsworn-progress-levels)))

  (let* ((track-id    (substring (secure-hash 'md5 name) 0 8))
         (track-prop  (format "ironsworn-progress-%s" track-id))
         (track-val   (format "\"%s\" %d %d" name level 0))

         (option      '(("At the same level as a sibling?" same-level)
                        ("As a subheading to this?" subheading)
                        ("No new heading. Re-use this?" no))))

    (when (called-interactively-p 'any)
      (cl-case (rpgdm-ironsworn--completing-read-value "Create a new heading? " option)
        (same-level (progn
                      (org-insert-heading-respect-content)
                      (insert name)))
        (subheading (progn
                      (org-insert-heading-respect-content)
                      (org-shiftmetaright)
                      (insert name)))))

    (org-set-property track-prop track-val)))

(defun rpgdm-ironsworn-progress-mark (name &optional times)
  "Mark progress against a track, NAME, storing result.
Instead of calling this function multiple times, you can specify
the number of TIMES to mark progress."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (unless times (setq times 1))
  (dotimes (_ times)
    (rpgdm-ironsworn-mark-progress-track name)))

(defun rpgdm-ironsworn--progress-amount (name)
  "Return list of aspects of progress by NAME.
Including:
  - track  :: The name of the track (same as NAME)
  - value  :: The complexity level, i.e. ticks per marked progress
  - level  :: The string label of a progress, e.g. Epic or Troublesome
  - ticks  :: The amount of ticks marked, at most 40
  - boxes  :: The number of completed boxes (ticks / 4, rounded down)
  - remain :: The number of remaining ticks marked towards the next box."
  (let* ((tracks  (rpgdm-ironsworn-character-progresses))
         (matched (assoc name tracks))
         (track   (car matched))
         (value   (cadr matched))
         (level   (rpgdm-ironsworn-progress-level-label value))
         (ticks   (min 40 (caddr matched)))   ; Older tracks may be over-marked
         (boxes   (/ ticks 4))
         (remain  (% ticks 4)))
    (list track value level ticks boxes remain)))

(defun rpgdm-ironsworn--progress-box (boxes leftover)
  "Return a org-like table of current completion of an Ironsworn progress.
For instance, with 4 boxes and 2 leftover tick marks, this will return:
| ■ | ■ | ■ | ■ | x |   |   |   |   |   | "
  (cl-labels ((make-box (boxes leftover-ticks blanks)
                (cond
                 ((> boxes 0) (concat " ■ |" (make-box (1- boxes) leftover-ticks blanks)))

                 ((> leftover-ticks 2) (concat " * |" (make-box 0 0 blanks)))
                 ((> leftover-ticks 1) (concat " x |" (make-box 0 0 blanks)))
                 ((> leftover-ticks 0) (concat " - |" (make-box 0 0 blanks)))

                 ((> blanks 0) (concat "   |" (make-box 0 0 (1- blanks)))))))

    (when (> leftover 3)
      (setq boxes    (+ boxes (/ leftover 4)))
      (setq leftover          (% leftover 4)))
    (when (< boxes 0)
      (setq boxes 0))
    (when (> boxes 10)
      (setq boxes 10))
    (let ((blanks (if (> leftover 0)
                      (- 9 boxes)
                    (- 10 boxes))))
      (concat "|" (make-box boxes leftover blanks)))))

(defun rpgdm-ironsworn-progress-amount (name)
  "Display the progress made against a track, NAME."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (cl-destructuring-bind
          (_track _value level _ticks boxes leftover)
          (rpgdm-ironsworn--progress-amount name)
        (if (not (called-interactively-p 'any))
                boxes
          (rpgdm-message "[%s] Progress on '%s': %d    %s" level name boxes
                         (rpgdm-ironsworn--progress-box boxes leftover)))))

(defun rpgdm-ironsworn-progress-roll (progress-value)
  "Display a Hit/Miss message based on the PROGRESS-VALUE.
This value is compared to rolling two d10 challenge dice."
  (interactive (list (rpgdm-ironsworn-progress-track-choose t)))
  (unless (numberp progress-value)
    (setq progress-value (rpgdm-ironsworn-progress-amount progress-value)))

  (let ((one-challenge (rpgdm--roll-die 10))
        (two-challenge (rpgdm--roll-die 10)))
    (rpgdm-message (rpgdm-ironsworn--results progress-value 0
                                             one-challenge two-challenge))))

(defun rpgdm-ironsworn-progress-delete (name)
  "Remove the progress track, NAME."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (save-excursion
    (-when-let ((prop) (rpgdm-ironsworn--goto-progress-track name))
      (org-delete-property prop))))

(defun rpgdm-ironsworn--table-key (name)
  "Return NAME as it appears in a table's file name.
For instance, `Sea Cave' becomes `sea-cave'."
  (downcase (string-replace " " "-" name)))

(rpgdm-tables-load (f-join rpgdm-ironsworn-project "tables"))

;; Loading the files directly (rather than with `rpgdm-tables-choose')
;; keeps a random choice out of the kill ring and echo area at load time:
(defvar rpgdm-ironsworn-site-themes
  (rpgdm-tables-load-file (f-join rpgdm-ironsworn-project "tables/site/theme.org")
                          "site/theme")
"A list of the Delve site themes.")

(defvar rpgdm-ironsworn-site-domains
  (rpgdm-tables-load-file (f-join rpgdm-ironsworn-project "tables/site/domain.org")
                          "site/domain")
"A list of the Delve site domains.")

(defun rpgdm-ironsworn-oracle-site-name (&optional place-type)
  "Return a randomly generated name for a dangerous site.
The PLACE-TYPE is something like `shadowfen' or `Sea Cave',
and helps to make the new name more meaningful to the place.
Domains without their own place table use the `unknown' table."
  (interactive (list (completing-read "Place type: " rpgdm-ironsworn-site-domains)))
  (unless place-type
    (setq place-type "unknown"))
  (let ((description (rpgdm-tables-choose "site/name/description"))
	(detail (rpgdm-tables-choose "site/name/detail"))
	(namesake (rpgdm-tables-choose "site/name/namesake"))
	(place  (or (rpgdm-tables-choose (format "site/name/place/%s"
                                                 (rpgdm-ironsworn--table-key place-type)))
                    (rpgdm-tables-choose "site/name/place/unknown")))
	(roll   (rpgdm--roll-die 100)))
    (rpgdm-message
     (cond
      ((<= roll 25) (format "%s %s" description place))
      ((<= roll 50) (format "%s of %s" place detail))
      ((<= roll 70) (format "%s of %s %s" place description detail))
      ((<= roll 80) (format "%s of %s's %s" place namesake detail))
      ((<= roll 85) (format "%s's %s" namesake place))
      ((<= roll 95) (format "%s %s of %s" description place namesake))
      (t            (format "%s of %s" place namesake))))))

(puthash "site/name" 'rpgdm-ironsworn-oracle-site-name rpgdm-tables)

(defun rpgdm-ironsworn-oracle-site-nature ()
  "Return a name and nature of a dangerous site.
The nature is a combination of theme and domain."
  (interactive)
  (let* ((theme  (rpgdm-tables-choose "site/theme"))
	 (domain (rpgdm-tables-choose "site/domain"))
	 (name   (rpgdm-ironsworn-oracle-site-name domain)))
    (rpgdm-message "%s %s :: %s" theme domain name)))

(puthash "site" 'rpgdm-ironsworn-oracle-site-nature rpgdm-tables)

(defun rpgdm-ironsworn-discover-a-site (theme domain &optional name level)
  "Store a Delve Site information in the org file.
The THEME and DOMAIN need to match org files in the `tables'
directory, and the choices themselves come from these files.
See the helper functions,
`rpgdm-ironsworn-site-themes' and `rpgdm-ironsworn-site-domains'.

Note, this function also queries the user for the name of the site
and progress level, and stores all this information in the org file,
as properties of a new heading for the site."
  (interactive
   (list
    (completing-read "What is the site theme? " rpgdm-ironsworn-site-themes)
    (completing-read "What is the domain? " rpgdm-ironsworn-site-domains)))

  (when (called-interactively-p 'any)
    (setq name (read-string "Site Name: " (rpgdm-ironsworn-oracle-site-name domain)))
    (setq level (rpgdm-ironsworn--completing-read-value "Progress Level: " rpgdm-ironsworn-progress-levels)))

  (org-insert-heading-respect-content)
  (org-shiftmetaright)
  (insert name)
  (rpgdm-ironsworn-progress-create name level)
  (rpgdm-ironsworn-store-character-temp-state 'site-theme (rpgdm-ironsworn--table-key theme))
  (rpgdm-ironsworn-store-character-temp-state 'site-domain (rpgdm-ironsworn--table-key domain))
  (forward-line 2))

(defun rpgdm-ironsworn-delve-the-depths-weak (stat)
  "Return random result from weak hit table for Delve the Depths.
The STAT should be the string, `wits', `shadow', or `edge'."
  (interactive (list (completing-read "Stat Choice: "
				      '("wits" "shadow" "edge"))))
  (let ((table-name (format "delve/weak-hit/%s" stat)))
    ;; (message "Rolling on %s" table-name)
    (rpgdm-tables-choose table-name)))

(defun rpgdm-ironsworn-delve-the-depths-weak-edge ()
  "Return random result from `edge` version of the weak hit table."
  (interactive)
  (rpgdm-ironsworn-delve-the-depths-weak "edge"))

(defun rpgdm-ironsworn-delve-the-depths-weak-shadow ()
  "Return random result from `shadow` version of the weak hit table."
  (interactive)
  (rpgdm-ironsworn-delve-the-depths-weak "shadow"))

(defun rpgdm-ironsworn-delve-the-depths-weak-wits ()
  "Return random result from `wits` version of the weak hit table."
  (interactive)
  (rpgdm-ironsworn-delve-the-depths-weak "wits"))

(defun rpgdm-ironsworn--reveal-a-danger ()
  "Return a random danger appropriate for Delve sites.
For instance, if the `danger' table states to consult the theme
or domain tables, this reads the `site-theme' and `site-domain'
properties from the site's heading (at or above point), and rolls on
the appropriate chart.  Without a site, the danger is returned as is."
  (let* ((state  (rpgdm-ironsworn-current-character-state))
         (theme  (gethash 'site-theme state))
         (domain (gethash 'site-domain state))
         (danger (rpgdm-tables-choose "danger")))
    (cond
     ;; Older org files may have stored names like `sea cave':
     ((and theme (equal danger "Check the theme card."))
      (rpgdm-tables-choose (format "danger/theme/%s" (rpgdm-ironsworn--table-key theme))))

     ((and domain (equal danger "Check the domain card."))
      (rpgdm-tables-choose (format "danger/domain/%s" (rpgdm-ironsworn--table-key domain))))

     ((s-starts-with? "Roll twice" danger)
      (format "%s <AND> %s"
              (rpgdm-ironsworn--reveal-a-danger)
              (rpgdm-ironsworn--reveal-a-danger)))

     (t   danger))))

(defun rpgdm-ironsworn-reveal-a-danger ()
  "Display a random danger appropriate for Delve sites.
For instance, if the `danger' table states to consult the theme
or domain tables, this reads the `site-theme' and `site-domain'
properties in the current org file, and rolls on the appropriate
chart."
  (interactive)
  (rpgdm-message "Revealed Danger: %s" (rpgdm-ironsworn--reveal-a-danger)))

(defun rpgdm-ironsworn-oracle-action-theme ()
  "Rolls on two tables at one time."
  (interactive)
  (let ((action (rpgdm-tables-choose "action"))
        (theme  (rpgdm-tables-choose "theme")))
    (rpgdm-message "%s / %s" action theme)))

(puthash "action-and-theme :: Roll on both tables"
         'rpgdm-ironsworn-oracle-action-theme
         rpgdm-tables)

(defun rpgdm-ironsworn-oracle-npc ()
  "Roll on all the character-related tables and show them together.
You'll need to pick and choose what works and discard what doesn't."
  (interactive)
  (let ((name        (rpgdm-tables-choose "name/ironlander"))
        (goal        (rpgdm-tables-choose "character/goal"))
        (role        (rpgdm-tables-choose "character/role"))
        (activity    (rpgdm-tables-choose "character/activity"))
        (description (rpgdm-tables-choose "character/descriptor"))
        (disposition (rpgdm-tables-choose "character/disposition")))
    (rpgdm-message "%s, %s %s (Activity: %s  Disposition: %s  Goal: %s)"
                   name description role activity disposition goal)))

(puthash "npc :: Roll on all character tables"
         'rpgdm-ironsworn-oracle-npc rpgdm-tables)

(defun rpgdm-ironsworn-oracle-combat ()
  "Return combat response combined from three combat tables."
  (interactive)
  (let ((action (rpgdm-tables-choose "combat/action"))
	(method (rpgdm-tables-choose "combat/event-method"))
	(target (rpgdm-tables-choose "combat/event-target")))
    (rpgdm-message "%s %s or %s" method target action)))

(puthash "combat" 'rpgdm-ironsworn-oracle-combat rpgdm-tables)

(defun rpgdm-ironsworn-oracle-feature ()
  "Roll on two tables at one time for a Site's feature."
  (interactive)
  (let ((aspect (rpgdm-tables-choose "feature/aspect"))
        (focus  (rpgdm-tables-choose "feature/focus")))
    (rpgdm-message "%s / %s" aspect focus)))

(puthash "feature-aspect-and-focus :: Roll on both feature tables"
         'rpgdm-ironsworn-oracle-feature rpgdm-tables)

(defun rpgdm-ironsworn-oracle-waypoint ()
  "Roll on two tables at one time for a Site's feature."
  (interactive)
  (let ((location     (rpgdm-tables-choose "location"))
        (description  (rpgdm-tables-choose "location-descriptors")))
    (rpgdm-message "%s %s" description (downcase location))))

(puthash "location-and-descriptor :: Roll on two tables for a waypoint"
         'rpgdm-ironsworn-oracle-waypoint rpgdm-tables)

(defvar rpgdm-ironsworn-oracle-threats '("Burgeoning Conflict" "Ravaging Horde"
                                         "Cursed Site" "Malignant Plague"
                                         "Scheming Leader" "Zealous Cult"
                                         "Environmental Calamity" "Power-Hungry Mystic"
                                         "Rampaging Creature")
  "A list of threats that correspond to tables.")

(defun rpgdm-ironsworn-oracle-threat-goal (&optional category)
  "Given a CATEGORY, display a threat goal."
  (interactive (list (completing-read "Threat: " rpgdm-ironsworn-oracle-threats)))
  (unless category
    (setq category (seq-random-elt rpgdm-ironsworn-oracle-threats)))
  (let ((table-name (format "threat/%s" (downcase (string-replace " " "-" category)))))
    (rpgdm-message "%s: %s" category (rpgdm-tables-choose table-name))))

(puthash "threat-goal :: Generate a goal for a particular threat"
         'rpgdm-ironsworn-oracle-threat-goal rpgdm-tables)

(defvar rpgdm-ironsworn-oracle-odds '((?c . 11) (?l . 26) (?h . 51) (?u . 76) (?n . 91))
  "Alist of odds key and the lowest d100 roll that answers yes.
Keys are Almost Certain, Likely, 50/50, Unlikely and Small Chance.")

(defun rpgdm-ironsworn--oracle-result (odds roll)
  "Return the Ask the Oracle answer for a ROLL on d100 at ODDS.
ODDS is a key from `rpgdm-ironsworn-oracle-odds'.  A match, where
both d10s show the same digit (11, 22, ... 99, 100), gives an
extreme result or a twist."
  (let ((answer (if (>= roll (alist-get odds rpgdm-ironsworn-oracle-odds)) "Yes" "No"))
        (match? (or (= roll 100) (= (% roll 11) 0))))
    (if match?
        (format "Extreme %s, or a twist" answer)
      answer)))

(defun rpgdm-ironsworn-oracle ()
  "Ask the Oracle a yes/no question, prompting for the odds."
  (interactive)
  (let* ((prompt "What are the odds?
  c) Almost Certain  l) Likely  h) 50/50  u) Unlikely  n) Small Chance ")
         (odds (read-char-choice prompt (mapcar #'car rpgdm-ironsworn-oracle-odds)))
         (roll (rpgdm--roll-die 100)))
    (rpgdm-message "%s (rolled %d, yes on %d+)"
                   (rpgdm-ironsworn--oracle-result odds roll)
                   roll (alist-get odds rpgdm-ironsworn-oracle-odds))))

(defun rpgdm-roll-d6 ()
  "Roll and display a six-sided die roll."
  (interactive)
  (rpgdm-roll "d6"))

(defun rpgdm-roll-d10 ()
  "Roll and display a ten-sided die roll."
  (interactive)
  (rpgdm-roll "d10"))

(defun rpgdm-roll-d100 ()
  "Roll and display a percentage die roll."
  (interactive)
  (rpgdm-roll "d100"))

(defun rpgdm-ironsworn-paste-last-move ()
  "Insert the contents of the `m' register, which should have last move."
  (interactive)
  (insert "\n" (get-register ?m)))

;; Optional packages bound in the main hydra:
(declare-function ace-link "ext:ace-link")
(declare-function evil-open-below "ext:evil-commands")

(defhydra hydra-rpgdm-oracles (:color blue)
  "Oracles"
  ("a" rpgdm-ironsworn-oracle-action-theme "Action/Theme")
  ("c" rpgdm-ironsworn-oracle-combat       "Combat")
  ("f" rpgdm-ironsworn-oracle-feature      "Feature")
  ("n" rpgdm-ironsworn-oracle-npc          "NPC")
  ("p" rpgdm-ironsworn-oracle-site-name    "Site Name")
  ("s" rpgdm-ironsworn-oracle-site-nature  "Site Nature")
  ("t" rpgdm-ironsworn-oracle-threat-goal  "Threat's Goal"))

(defhydra hydra-rpgdm-delve (:color blue)
  "Delve site actions"
  ("n" rpgdm-ironsworn-discover-a-site "discover a site")
  ("w" rpgdm-ironsworn-delve-the-depths-weak "weak hit table")
  ("d" rpgdm-ironsworn-reveal-a-danger "reveal a danger")
  ("m" rpgdm-ironsworn-progress-mark   "mark progress")
  ("p" rpgdm-ironsworn-progress-amount "show progress")
  ("x" rpgdm-ironsworn-progress-delete "delete")
  ("r" rpgdm-ironsworn-progress-roll   "locate your objective"))

(defhydra hydra-rpgdm-progress (:color blue)
  "Progress Tracks"
  ("n" rpgdm-ironsworn-progress-create "new")
  ("m" rpgdm-ironsworn-progress-mark   "mark")
  ("p" rpgdm-ironsworn-progress-amount "show")
  ("x" rpgdm-ironsworn-progress-delete "delete")
  ("r" rpgdm-ironsworn-progress-roll   "roll"))

(defhydra hydra-rpgdm-assets (:color blue)
  "Progress Tracks"
  ("A" rpgdm-ironsworn-insert-character-asset "insert new asset")
  ("a" rpgdm-ironsworn-asset-stat-adjust "adjust asset stat")
  ("n" rpgdm-ironsworn-asset-stat-create "new asset stat")
  ("s" rpgdm-ironsworn-asset-stat-show   "show asset stat")
  ("r" rpgdm-ironsworn-asset-stat-roll "roll asset as modifier")
  ("v" rpgdm-ironsworn-show-character-asset "view asset"))

(defhydra hydra-rpgdm (:color pink :hint nil)
  "
    ^Dice^     0=d100 1=d10 6=d6         ^Roll/Adjust^   ^Oracles/Tables^       ^Moving/Editing^      ^Messages^
 ------------------------------------------------------------------------------------------------------------------------------
    _D_: Roll Dice  _h_: Roll Shadow       _l_/_L_: Health   _z_/_Z_: Yes/No Oracle   _o_: Links            ⌘-h: Show Stats
    _e_: Roll Edge  _w_: Roll Wits         _t_/_T_: Spirit   _c_/_C_: Show Oracle     _J_/_K_: Page up/dn     ⌘-l: Last Results
    _r_: Roll Heart _p_/_a_: Progress/Assets _s_/_S_: Supply     _O_/_g_: Oracles/Load  _N_/_W_: Narrow/Widen   ⌘-k: ↑ Previous
    _i_: Roll Iron  _m_: Make Move           _M_: Momentum   _d_: Delve Actions   _y_/_Y_: Yank/Move      ⌘-j: ↓ Next   "
  ("D" rpgdm-ironsworn-roll)
  ("z" rpgdm-ironsworn-oracle)  ("Z" rpgdm-yes-and-50/50)

  ("e" rpgdm-ironsworn-roll-edge)
  ("r" rpgdm-ironsworn-roll-heart)
  ("i" rpgdm-ironsworn-roll-iron)
  ("h" rpgdm-ironsworn-roll-shadow)
  ("w" rpgdm-ironsworn-roll-wits)
  ("m" rpgdm-ironsworn-make-move :color pink)

  ("l" rpgdm-ironsworn-roll-health)
  ("L" rpgdm-ironsworn-adjust-health :color pink)
  ("t" rpgdm-ironsworn-roll-spirit)
  ("T" rpgdm-ironsworn-adjust-spirit :color pink)
  ("s" rpgdm-ironsworn-roll-supply)
  ("S" rpgdm-ironsworn-adjust-supply :color pink)
  ("M" rpgdm-ironsworn-adjust-momentum :color pink)

  ("O" hydra-rpgdm-oracles/body :color blue)
  ("g" rpgdm-tables-load)       ("c" rpgdm-tables-choose)     ("C" rpgdm-tables-choose :color pink)

  ("d" hydra-rpgdm-delve/body :color blue)
  ("p" hydra-rpgdm-progress/body :color blue)
  ("a" hydra-rpgdm-assets/body :color blue)

  ("o" ace-link)                 ("N" org-narrow-to-subtree)   ("W" widen)
  ("K" scroll-down :color pink)  ("J" scroll-up :color pink)
  ("y" rpgdm-paste-last-message) ("Y" rpgdm-ironsworn-paste-last-move)
  ("s-h" rpgdm-ironsworn-character-display)
  ("C-n" rpgdm-last-results-next :color pink)
  ("C-p" rpgdm-last-results-previous :color pink)
  ("s-l" rpgdm-last-results :color pink)
  ("s-j" rpgdm-last-results-next :color pink)
  ("s-k" rpgdm-last-results-previous :color pink)

  ("0" rpgdm-roll-d100 :color pink)
  ("1" rpgdm-roll-d10 :color pink)
  ("6" rpgdm-roll-d6 :color pink)
  ("RET" evil-open-below :color blue)
  ("q" nil "quit") ("<f6>" nil))

(defun rpgdm-ironsworn-store-character-temp-state (stat value)
  "Store the VALUE of a character's STAT in the current org tree property.
Note that STAT should be a symbol, like `supply' and VALUE should be a
number, but doesn't have to be."
  (let ((prop (format "ironsworn-%s" (symbol-name stat))))
    (when (numberp value)
      (setq value (number-to-string value)))
    (org-set-property prop value)))

(defun rpgdm-ironsworn-store-character-state (stat value)
  "Store the VALUE of a character's STAT in the top-level org tree property.
Note that STAT should be a symbol, like `supply' and VALUE should be a
number, but doesn't have to be."
  (save-excursion
    (rpgdm-ironsworn--top-heading)
    (rpgdm-ironsworn-store-character-temp-state stat value)))

(defalias 'rpgdm-ironsworn-store-default-character-state
          'rpgdm-ironsworn-store-character-state)

(defun rpgdm-ironsworn--property-p (prop)
  "Given a symbol PROP, return non-nil if it is an ironsworn keyword.
Specifically, does it begin with `:IRONSWORN-'"
  (let ((p (symbol-name prop)))
    (string-match (rx bos ":IRONSWORN-") p)))

(defun rpgdm-ironsworn--progress-p (prop)
  "Given a symbol PROP, return non-nil if it is an ironsworn progress keyword.
Specifically, does it begin with `:IRONSWORN-PROGRESS'"
  (let ((p (symbol-name prop)))
    (string-match (rx bos ":IRONSWORN-PROGRESS-") p)))

(defun rpgdm-ironsworn--short-progress-p (prop)
  "Return non-nil if symbol, PROP, begins with progress."
  (let ((p (symbol-name prop)))
    (s-starts-with-p "progress-" p)))

(defun rpgdm-ironsworn--progress-to-str (prop)
  "Convert a progress symbol, PROP to a string."
  (downcase (substring (symbol-name prop) 1)))

(defun rpgdm-ironsworn--top-heading ()
  "Move point to the top-most ancestor heading of the current entry."
  (org-back-to-heading t)
  (while (org-up-heading-safe)))

(defun rpgdm-ironsworn--heading-properties ()
  "Return the current heading's own properties as a plist.
Keys are keywords like `:IRONSWORN-EDGE', and values are strings.
Inherited properties are not included."
  (cl-loop for (key . value) in (org-entry-properties nil 'standard)
           append (list (intern (concat ":" (upcase key))) value)))

(defun rpgdm-ironsworn--current-character-state (results)
  "Insert the current heading's properties, and its ancestors', in RESULTS.
If a property is already in the hash table, RESULTS, it is not
overwritten, thereby having lower-level subtrees take precedence
over similar settings in higher headers."
  (cl-flet ((key-convert (ironsworn-prop)
              (make-symbol (downcase (substring (symbol-name ironsworn-prop) 11))))
            (value-convert (value)
              (if (string-match (rx bos (optional "-") (one-or-more digit) eos) value)
                  (string-to-number value)
                value)))
    (cl-loop do
             (cl-loop for (k v) on (rpgdm-ironsworn--heading-properties) by (function cddr) do
                      ;; If key is ironsworn property, but isn't in the table...
                      (when (rpgdm-ironsworn--property-p k)
                        (let ((key (key-convert k))
                              (val (value-convert v)))
                          (unless (gethash key results)
                            (puthash key val results)))))
             while (org-up-heading-safe))))

(defun rpgdm-ironsworn-current-character-state ()
  "Return all set properties based on cursor position in org doc.
Note that values in sibling trees are ignored, and settings in
lower levels of the tree headings take precedence."
  (save-excursion
    (let ((results (make-hash-table :test 'str-or-keys)))
      (org-back-to-heading t)

      ;; Put the lowest heading title in the results hashtable:
      (puthash 'title (org-element-property :raw-value (org-element-at-point))
               results)
      (rpgdm-ironsworn--current-character-state results)
      results)))

(defun rpgdm-ironsworn--progress-values (value)
  "Parse a string VALUE returning a list of parts.
This includes the initial name, the number of ticks to mark,
and the current progress of the track."
  (let ((regxp (rx "\""
                  (group (one-or-more (not "\"")))
                  "\"" (one-or-more space)
                  (group (one-or-more digit))
                  (one-or-more space)
                  (group (one-or-more digit)))))
    (when (string-match regxp value)
      (list (match-string 1 value)
            (string-to-number (match-string 2 value))
            (string-to-number (match-string 3 value))))))

(defun rpgdm-ironsworn-character-progresses ()
  "Return a list where each element is progress track list.
Each sublist has three values, a name, the ticks to mark,
and the current progress of the track."
  (let ((state (rpgdm-ironsworn-current-character-state)))
    (thread-last state
                 (hash-table-keys)
                 (-filter #'rpgdm-ironsworn--short-progress-p)
                 (--map (gethash it state))
                 (-map #'rpgdm-ironsworn--progress-values))))

(defun rpgdm-ironsworn--goto-progress-track (name)
  "Move point to the heading that stores the progress track, NAME.
Search the current heading, then its ancestors.  Return a list of
the track's property name and its parsed values (see
`rpgdm-ironsworn--progress-values'), or nil if not found."
  (org-back-to-heading t)
  (cl-loop thereis
           (cl-loop for (k v) on (rpgdm-ironsworn--heading-properties) by (function cddr)
                    for values = (and (rpgdm-ironsworn--progress-p k)
                                      (rpgdm-ironsworn--progress-values v))
                    when (equal name (car values))
                    return (list (rpgdm-ironsworn--progress-to-str k) values))
           while (org-up-heading-safe)))

(defun rpgdm-ironsworn--mark-progress-track (str)
  "Given a progress track's name, STR, update its progress mark."
  (-when-let ((prop (label level progress)) (rpgdm-ironsworn--goto-progress-track str))
    (org-set-property prop (format "\"%s\" %d %d"
                                   label level (min 40 (+ level progress))))))

(defun rpgdm-ironsworn-mark-progress-track (label)
  "Given a progress track's name, LABEL, update its progress mark."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (save-excursion
    (rpgdm-ironsworn--mark-progress-track label)))

(provide 'rpgdm-ironsworn)
;;; rpgdm-ironsworn.el ends here
