(require 'rpgdm)

(defvar rpgdm-ironsworn-project (file-name-directory load-file-name)
  "The root directory to the emacs-ironsworn project")

(defun rpgdm-ironsworn--results (action modifier one-challenge two-challenge)
  (let* ((action-results (+ action modifier))
         (str-results (cond
                       ((and (> action-results one-challenge) (> action-results two-challenge))
                        (propertize "Strong hit" 'face '(:foreground "green")))
                       ((or  (> action-results one-challenge) (> action-results two-challenge))
                        (propertize "Weak hit" 'face '(:foreground "yellow")))
                       (t (propertize "Miss" 'face '(:foreground "red")))))
         (matched-msg  (if (= one-challenge two-challenge)
                           (propertize " ← Create a Twist" 'face '(:foreground "orange"))
                         "")))
    (format "%s %s %d %s%d %s %d%s %s %d %s %d %s" str-results
            (propertize "::" 'face '(:foreground "#888"))
            (+ action modifier)
            (propertize "(" 'face '(:foreground "#888"))
            action
            (propertize "+" 'face '(:foreground "#888"))
            modifier
            (propertize ")" 'face '(:foreground "#888"))
            (propertize "→" 'face '(:foreground "light blue"))
            one-challenge
            (propertize "/" 'face '(:foreground "#888"))
            two-challenge matched-msg)))

(defun rpgdm-ironsworn-roll (modifier)
  "Display a Hit/Miss message based on comparing a d6 action
roll (added to MODIFIER) vs. two d10 challenge dice."
  (interactive "nModifier: ")
  (let ((one-challenge (rpgdm--roll-die 10))
        (two-challenge (rpgdm--roll-die 10))
        (action-roll   (rpgdm--roll-die 6)))
    (rpgdm-message (rpgdm-ironsworn--results action-roll modifier
                                             one-challenge two-challenge))))

(defun rpgdm-ironsworn-new-character ()
  "Interactively query the user for a new character's attribute.
This function _appends_ this information to the current buffer,
which should be using the `org-mode' major mode."
  (interactive)
  (let ((name (read-string "What is the new character's name? "))
        (frmt (seq-random-elt '("* The Adventures of %s"
                                "* The Journeys of %s"
                                "* %s, an Epic Saga"
                                "* The Epic of %s"
                                "* Travels of %s"))))
    (when (s-blank? name)
      (setq name (rpgdm-tables-choose "names-ironlander")))
    (goto-char (point-max))
    (insert "# Local Variables:
# eval: (progn (require 'rpgdm-ironsworn) (rpgdm-mode) (rpgdm-tables-load (concat rpgdm-ironsworn-project \"tables\")))
# End:
")
    (insert (format frmt name))

    (dolist (stat '(edge heart iron shadow wits))
      (rpgdm-ironsworn-store-character-state stat
                                             (read-string (format "What is %s's %s stat: " name stat))))

    (dolist (stat '(health spirit supply))
      (rpgdm-ironsworn-store-character-state stat 5))
    (rpgdm-ironsworn-store-character-state 'momentum 2)

    (rpgdm-ironsworn-progress-create "Bonds" 1)
    (rpgdm-ironsworn-progress-create (read-string "What title should we give this new character's Epic vow? ") 1)
    (message "Alright, the template for %s is complete. Edit away!" name)))

(defun rpgdm-ironsworn--display-stat (stat character)
  (let* ((value (gethash stat character))
         (s-val (number-to-string value))
         (color (cond
                 ((< value 1) "red")
                 ((< value 3) "orange")
                 ((< value 4) "yellow")
                 (t "green"))))
    (propertize s-val 'face `(:foreground ,color))))

(defun rpgdm-ironsworn-character-display ()
  "Easily display the character's stats and other things."
  (interactive)
  (let ((character (rpgdm-ironsworn-current-character-state)))
    (rpgdm-message "Edge: %d  Heart: %d  Iron: %d  Shadow: %d  Wits: %d
Health: %s  Spirit: %s  Supply: %s  Momentum: %d"
                   (rpgdm-ironsworn-character-stat 'edge character)
                   (rpgdm-ironsworn-character-stat 'heart character)
                   (rpgdm-ironsworn-character-stat 'iron character)
                   (rpgdm-ironsworn-character-stat 'shadow character)
                   (rpgdm-ironsworn-character-stat 'wits character)

                   (rpgdm-ironsworn--display-stat 'health character)
                   (rpgdm-ironsworn--display-stat 'spirit character)
                   (rpgdm-ironsworn--display-stat 'supply character)

                   (gethash 'momentum character 5))))

(defun rpgdm-ironsworn-to-string (a)
  "Return a lowercase string from either a string, keyword or symbol."
  (downcase
   (cond
    ((keywordp a) (substring (symbol-name a) 1))
    ((symbolp  a) (symbol-name a))
    (t            a))))

(define-hash-table-test 'str-or-keys
  (lambda (a b)
    (string-equal (rpgdm-ironsworn-to-string a) (rpgdm-ironsworn-to-string b)))

  (lambda (s) (sxhash-equal (rpgdm-ironsworn-to-string s))))

(defun rpgdm-ironsworn-character-stat (stat &optional character)
  "Return integer value associated with a character's STAT."
  (when (null character)
    (setq character (rpgdm-ironsworn-current-character-state)))
  (gethash stat character 1))

(defun rpgdm-ironsworn-adjust-stat (stat adj &optional default)
  "Increase or decrease the current character's STAT by ADJ."
  (let ((value (+ (gethash stat rpgdm-ironsworn-character default) adj)))
    ;; TODO: Delete this hash bidness
    (puthash stat value rpgdm-ironsworn-character)
    (rpgdm-ironsworn-store-character-state stat value)))

(defun rpgdm-ironsworn-adjust-health (health-adj)
  "Increase or decrease the current character's health by HEALTH-ADJ."
  (interactive "nHealth Adjustment: ")
  (rpgdm-ironsworn-adjust-stat 'health health-adj 5))

(defun rpgdm-ironsworn-adjust-spirit (spirit-adj)
  "Increase or decrease the current character's spirit by SPIRIT-ADJ."
  (interactive "nSpirit Adjustment: ")
  (rpgdm-ironsworn-adjust-stat 'spirit spirit-adj 5))

(defun rpgdm-ironsworn-adjust-supply (supply-adj)
  "Increase or decrease the current character's supply by SUPPLY-ADJ."
  (interactive "nSupply Adjustment: ")
  (rpgdm-ironsworn-adjust-stat 'supply supply-adj 5))

(defun rpgdm-ironsworn-adjust-momentum (momentum-adj)
  "Increase or decrease the current character's momentum by MOMENTUM-ADJ."
  (interactive "nMomentum Adjustment: ")
  (rpgdm-ironsworn-adjust-stat 'momentum momentum-adj 2))

(defun rpgdm-ironsworn-roll-stat (stat modifier)
  "Roll an action based on a loaded character's STAT with a MODIFIER."
  (interactive (list (completing-read "Stat Modifier: " '(Edge Heart Iron Shadow Wits))
                     (read-string "Other Modifier: ")))
  (let ((all-mods (+ (rpgdm-ironsworn-character-stat stat)
                     (string-to-number modifier))))
    (rpgdm-ironsworn-roll all-mods)))

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

(defun rpgdm-ironsworn--move-tuple (file)
  (let* ((regx (rx "moves/"
                   (group (one-or-more (not "/")))
                   "/"
                   (group (one-or-more (not ".")))
                   ".org" eol))
         (mtch (string-match regx file))
         (type (match-string 1 file))
         (name (thread-last (match-string 2 file)
                 (s-replace-regexp "-" " ")
                 (s-titleize))))
    (list (format "%s:: %s" type name) file)))

(defvar rpgdm-ironsworn-moves () "A list of tuples of the move and the file containing its goodness.")

(defun rpgdm-ironsworn-moves ()
  "Return a list containing available moves, and the filename containing
  the moves instructions, and other properties. Note that this function is
  memoized, in that re-calling this function will return a cached copy."
  (unless rpgdm-ironsworn-moves
    (setq rpgdm-ironsworn-moves
          (mapcar 'rpgdm-ironsworn--move-tuple
                  (directory-files-recursively
                   (concat rpgdm-ironsworn-project "moves")
                   ".*\.org$"))))
  rpgdm-ironsworn-moves)

(defun rpgdm-ironsworn-choose-move ()
  (let* ((move (completing-read "Move: " (rpgdm-ironsworn-moves)))
         (tuple (assoc move (rpgdm-ironsworn-moves))))
    (cadr tuple)))

(defun rpgdm-ironsworn--store-move (title results)
  "Store the results in a `m' register. It should also include
the name of the move, based on the current file."
  (set-register ?m (format "# %s ... %s " title results)))

(defun rpgdm-ironsworn-make-move (move-file)
  "Make an Ironsworn move by loading MOVE-FILE, and optionally querying the
user to make an initial roll based on the properties in the file."
  (interactive (list (rpgdm-ironsworn-choose-move)))

  ;; Normally, we'd call `save-window-excursion', however, that buries the file
  ;; we show, and I think we should leave it up for study.
  (let (props title
        (orig-buf (window-buffer)))
    (find-file-other-window move-file)
    (goto-char (point-min))
    (setq title (cdr (assoc "ITEM" (org-entry-properties))))
    (setq props (first (org-property-values "move-stats")))
    (pop-to-buffer orig-buf)
    (rpgdm-ironsworn--store-move title (rpgdm-ironsworn--make-move props))))

(defun rpgdm-ironsworn--make-move (move-props)
  "Query user for rolls based on the MOVE-PROPS."
 (let* ((props (s-split " " (or move-props "")))
        (count (seq-length props))
        (stats (seq-filter (lambda (s) (string-match (rx (one-or-more alpha)) s)) props))
        (first (first props)))
   (cond
    ((seq-empty-p stats)       nil)
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

(defvar rpgdm-ironsworn-progress-levels '(("Troublesome" . 12)
                                          ("Dangerous" . 8) ("Formidable" . 4)
                                          ("Extreme" . 2) ("Epic" . 1))
  "The five levels of progression for an Ironsworn progress track.")

(defun rpgdm-ironsworn-progress-level (label)
  "Return the level (number of ticks to mark) of progress LABEL.
For instance, if LABEL is `Dangerous', this returns `8'."
  (alist-get label rpgdm-ironsworn-progress-levels 8 nil 'equal))

(defun rpgdm-ironsworn-progress-level-label (level)
  "Return the label associated with the progress LEVEL, e.g. dangerous."
  (car (rassoc level rpgdm-ironsworn-progress-levels)))

(defun rpgdm-ironsworn-progress-track-choose (&optional allow-other)
  "Query the user for a particular stored track. If ALLOW-OTHER is non-nil,
we append a choice, <other>, which allows the user to choose the number
of squares that have been marked against some progress."
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
Stored as a property in the org file. Keep in mind that the
NAME should be a short title, not a description."
  (interactive (list (read-string "Progress Name: ")
                     (completing-read "Progress Level: "
                                      rpgdm-ironsworn-progress-levels)))

  (let* ((level-value (rpgdm-ironsworn-progress-level level))
         (track-id    (substring (secure-hash 'md5 name) 0 8))
         (track-prop  (format "ironsworn-progress-%s" track-id))
         (track-val   (format "\"%s\" %d %d" name level-value 0)))
    (org-set-property track-prop track-val)))

(defun rpgdm-ironsworn-progress-mark (name &optional times)
  "Mark progress against a track, NAME. Instead of calling this function multiple
times, you can specify the number of TIMES to mark progress."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (unless times (setq times 1))
  (dotimes (idx times)
    (rpgdm-ironsworn-mark-progress-track name)))

(defun rpgdm-ironsworn-progress-amount (name)
  "Display the progress made against a track, NAME."
  (interactive (list (rpgdm-ironsworn-progress-track-choose)))
  (let* ((tracks  (rpgdm-ironsworn-character-progresses))
         (matched (--filter (equal name (first it)) tracks))
         (track   (first matched))
         (value   (second track))
         (level   (rpgdm-ironsworn-progress-level-label value))
         (ticks   (third track))
         (boxes   (/ ticks 4)))
    (if (called-interactively-p 'any)
        (rpgdm-message "[%d] Progress on %s: %d  (Ticks: %d)" level name boxes ticks)
      boxes)))

(defun rpgdm-ironsworn-progress-roll (progress-value)
  "Display a Hit/Miss message based on comparing the PROGRESS-VALUE
to rolling two d10 challenge dice."
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
  (let ((tracks      (gethash 'progress-tracks rpgdm-ironsworn-character)))
    (ignore-errors
      (remhash name tracks))))

(defun rpgdm-ironsworn-oracle-action-theme ()
  "Rolls on two tables at one time."
  (interactive)
  (let ((action (rpgdm-tables-choose "actions"))
        (theme  (rpgdm-tables-choose "themes")))
    (rpgdm-message "%s / %s" action theme)))

(defun rpgdm-ironsworn-oracle-npc ()
  "Roll on all the character-related tables and show them together.
You'll need to pick and choose what works and discard what doesn't."
  (interactive)
  (let ((name        (rpgdm-tables-choose "names-ironlander"))
        (goal        (rpgdm-tables-choose "character-goal"))
        (role        (rpgdm-tables-choose "character-role"))
        (activity    (rpgdm-tables-choose "character-activity"))
        (description (rpgdm-tables-choose "character-descriptor"))
        (disposition (rpgdm-tables-choose "character-disposition")))
    (rpgdm-message "%s, %s %s (Activity: %s  Disposition: %s  Goal: %s)"
                   name description role activity disposition goal)))

(defun rpgdm-ironsworn-oracle-combat ()
  (interactive)
  (let ((action (rpgdm-tables-choose "combat-action"))
        (method (rpgdm-tables-choose "combat-event-method"))
        (target (rpgdm-tables-choose "combat-event-target")))
    (rpgdm-message "%s %s or %s" method target action)))

(defun rpgdm-ironsworn-oracle-feature ()
  "Rolls on two tables at one time for a Site's feature."
  (interactive)
  (let ((aspect (rpgdm-tables-choose "feature-aspect"))
        (focus  (rpgdm-tables-choose "feature-focus")))
    (rpgdm-message "%s / %s" aspect focus)))

(defun rpgdm-ironsworn-oracle-site-nature ()
  "Rolls on two tables at one time for a random Site."
  (interactive)
  (let* ((theme  (rpgdm-tables-choose "site-theme"))
         (domain (rpgdm-tables-choose "site-domain"))
         (place  (downcase domain))
         (name   (rpgdm-ironsworn-oracle-site-name place)))
    (rpgdm-message "%s %s :: %s" theme domain name)))

(defun rpgdm-ironsworn-oracle-site-name (&optional place-type)
  "Rolling on multiple tables to return a random site name."
  (interactive (list (completing-read "Place type: "
                                      '(barrow cavern icereach mine pass ruin
                                               sea-cave shadowfen stronghold
                                               tanglewood underkeep))))
  (unless place-type
    (setq place-type "unknown"))
  (let ((description (rpgdm-tables-choose "site-name-description"))
        (detail (rpgdm-tables-choose "site-name-detail"))
        (namesake (rpgdm-tables-choose "site-name-namesake"))
        (place  (rpgdm-tables-choose (format "site-name-place-%s" place-type)))
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

(defvar rpgdm-ironsworn-oracle-threats '("Burgeoning Conflict" "Ravaging Horde"
                                         "Cursed Site" "Malignant Plague"
                                         "Scheming Leader" "Zealous Cult"
                                         "Environmental Calamity" "Power-Hungry Mystic"
                                         "Rampaging Creature")
  "A list of threats that correspond to tables")

(defun rpgdm-ironsworn-oracle-threat-goal (&optional category)
  "Given a CATEGORY, display a threat goal."
  (interactive (list (completing-read "Threat: " rpgdm-ironsworn-oracle-threats)))
  (unless category
    (setq category (seq-random-elt rpgdm-ironsworn-oracle-threats)))
  (let ((table-name (format "threat-%s" (downcase (string-replace " " "-" category)))))
    (rpgdm-message "%s: %s" category (rpgdm-tables-choose table-name))))

(defun rpgdm-ironsworn-oracle ()
  "Given a LIKLIHOOD as a single character, return weighted coin flip."
  (interactive)
  (let* ((prompt "What are the odds?
  c) Almost Certain  l) Likely  h) 50/50  u) Unlikely  n) Small Chance ")
         (odds (read-char prompt))
         (roll (rpgdm--roll-die 100))
         (yes! (when (or (and (= roll 11) (eq odds ?c))
                         (and (= roll 26) (eq odds ?l))
                         (and (= roll 51) (eq odds ?h))
                         (and (= roll 76) (eq odds ?u))
                         (and (= roll 91) (eq odds ?n)))
                 t))
         (yes  (when (or (and (> roll 11) (eq odds ?c))
                         (and (> roll 26) (eq odds ?l))
                         (and (> roll 51) (eq odds ?h))
                         (and (> roll 76) (eq odds ?u))
                         (and (> roll 91) (eq odds ?n)))
                 t)))
    (rpgdm-message "%s %s %s"
                   (if yes! "Extreme" "")
                   (if yes "Yes" "No")
                   (if yes! "or a twist." ""))))

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
  (insert "\n" (get-register ?m)))

(defhydra hydra-rpgdm-oracles (:color blue)
  "Oracles"
  ("a" rpgdm-ironsworn-oracle-action-theme "Action/Theme")
  ("c" rpgdm-ironsworn-oracle-combat       "Combat")
  ("f" rpgdm-ironsworn-oracle-feature      "Feature")
  ("n" rpgdm-ironsworn-oracle-npc          "NPC")
  ("p" rpgdm-ironsworn-oracle-site-name    "Site Name")
  ("s" rpgdm-ironsworn-oracle-site-nature  "Site Nature")
  ("t" rpgdm-ironsworn-oracle-threat-goal  "Threat's Goal"))

(defhydra hydra-rpgdm-progress (:color blue)
  "Progress Tracks"
  ("n" rpgdm-ironsworn-progress-create "new")
  ("m" rpgdm-ironsworn-progress-mark   "mark")
  ("p" rpgdm-ironsworn-progress-amount "show")
  ("d" rpgdm-ironsworn-progress-delete "delete")
  ("r" rpgdm-ironsworn-progress-roll   "roll"))

(defhydra hydra-rpgdm (:color blue :hint nil)
  "
    ^Dice^     0=d100 1=d10 6=d6       ^Adjust^      ^Oracles/Tables^     ^Moving/Editing^      ^Messages^
 ----------------------------------------------------------------------------------------------------------------------------------------------------
    _d_: Roll Dice  _p_: Progress       _H_: Health   _z_: Yes/No Oracle   _o_: Links            ⌘-h: Show Stats
    _e_: Roll Edge  _s_: Roll Shadow    _S_: Spirit   _c_: Show Oracle     _J_/_K_: Page up/dn     ⌘-l: Last Results
    _h_: Roll Heart _w_: Roll Wits      _G_: Supply   _O_: Other Oracles   _N_/_W_: Narrow/Widen   ⌘-k: ↑ Previous
    _i_: Roll Iron  _m_: Make Move      _M_: Momentum _T_: Load Oracles    _y_/_Y_: Yank/Move      ⌘-j: ↓ Next   "
  ("d" rpgdm-ironsworn-roll)    ("D" rpgdm-ironsworn-progress-roll)
  ("z" rpgdm-ironsworn-oracle)  ("O" rpgdm-oracle)

  ("e" rpgdm-ironsworn-roll-edge)
  ("h" rpgdm-ironsworn-roll-heart)
  ("i" rpgdm-ironsworn-roll-iron)
  ("s" rpgdm-ironsworn-roll-shadow)
  ("w" rpgdm-ironsworn-roll-wits)
  ("m" rpgdm-ironsworn-make-move :color pink)

  ("H" rpgdm-ironsworn-adjust-health :color pink)
  ("S" rpgdm-ironsworn-adjust-spirit :color pink)
  ("G" rpgdm-ironsworn-adjust-supply :color pink)
  ("M" rpgdm-ironsworn-adjust-momentum :color pink)

  ("T" rpgdm-tables-load)       ("c" rpgdm-tables-choose)     ("C" rpgdm-tables-choose :color pink)
  ("O" hydra-rpgdm-oracles/body)
  ("p" hydra-rpgdm-progress/body)

  ("o" ace-link)                 ("N" org-narrow-to-subtree)   ("W" widen)
  ("K" scroll-down :color pink)  ("J" scroll-up :color pink)
  ("y" rpgdm-paste-last-message) ("Y" rpgdm-ironsworn-paste-last-move)
  ("s-h" rpgdm-ironsworn-character-display)
  ("C-m" rpgdm-last-results :color pink)
  ("C-n" rpgdm-last-results-next :color pink)
  ("C-p" rpgdm-last-results-previous :color pink)
  ("s-l" rpgdm-last-results :color pink)
  ("s-j" rpgdm-last-results-next :color pink)
  ("s-k" rpgdm-last-results-previous :color pink)

  ("0" rpgdm-roll-d100 :color pink)
  ("1" rpgdm-roll-d10 :color pink)
  ("6" rpgdm-roll-d6 :color pink)
  ("q" nil "quit") ("<f6>" nil))

(defun rpgdm-ironsworn-store-character-state (stat value)
  "Store the VALUE of a character's STAT in the current org tree property.
Note that STAT should be a symbol, like `supply' and VALUE should be a
number, but doesn't have to be."
  (let ((prop (format "ironsworn-%s" (symbol-name stat))))
    (when (numberp value)
      (setq value (number-to-string value)))
    (org-set-property prop value)))

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
  (let ((p (symbol-name prop)))
    (s-starts-with-p "progress-" p)))

(defun rpgdm-ironsworn--progress-to-str (prop)
  "Convert a progress symbol, PROP to a string."
  (downcase (substring (symbol-name prop) 1)))

(defun org-heading-level ()
  "Return heading level of the element at the point. 0 otherwise."
  (if-let ((level-str (org-element-property :level (org-element-at-point))))
      level-str
    0))

(defun rpgdm-ironsworn--current-character-state (results)
  "Recursive helper to insert current header properties in RESULTS.
Calls itself if it is not looking at the top-level header in the
file. If a property is already in the hash table, RESULTS, it is
not overwritten, thereby having lower-level subtrees take
precendence over similar settings in higher headers."
  (defun key-convert (ironsworn-prop)
    (make-symbol (downcase (substring (symbol-name ironsworn-prop) 11))))

  (defun value-convert (value)
    (if (string-match (rx bos (one-or-more digit) eos) value)
        (string-to-number value)
      value))

  (let ((props (org-element--get-node-properties)))
    (loop for (k v) on props by (function cddr) do
          ;; If key is ironsworn property, but isn't in the table...
          (when (rpgdm-ironsworn--property-p k)
            (let ((key (key-convert k))
                  (val (value-convert v)))
              (unless (gethash key results)
                (puthash key val results)))))

    (unless (= (org-heading-level) 1)
      (org-up-element)
      (rpgdm-ironsworn--current-character-state results))))

(defun rpgdm-ironsworn-current-character-state ()
  "Return all set properties based on cursor position in org doc.
Note that values in sibling trees are ignored, and settings in
lower levels of the tree headings take precedence."
  (save-excursion
    (let ((results (make-hash-table :test 'str-or-keys)))
      (unless (eq 'headline (org-element-type (org-element-at-point)))
        (org-up-element))

      (rpgdm-ironsworn--current-character-state results)
      results)))

(defun rpgdm-ironsworn--progress-values (value)
  "Parse a string VALUE returning a list of parts,
Including the initial name, the number of ticks to mark,
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

(defun rpgdm-ironsworn--mark-progress-track (str)
  "Given a progress track's name, STR, update its progress mark."
  (let ((props (org-element--get-node-properties)))
    (loop for (k v) on props by (function cddr) do
          (when (rpgdm-ironsworn--progress-p k)
            (-let* (((label level progress) (rpgdm-ironsworn--progress-values v)))
              (when (equal str label)
                (org-set-property (rpgdm-ironsworn--progress-to-str k)
                                  (format "\"%s\" %d %d"
                                          label level (+ level progress)))
                (cl-return))))))

  ;; Did not find a match at this level, let's pop up one and try again:
  (unless (= (org-heading-level) 1)
    (org-up-element)
    (rpgdm-ironsworn--mark-progress-track str)))

(defun rpgdm-ironsworn-mark-progress-track (label)
  "Given a progress track's name, STR, update its progress mark."
  (interactive (list (completing-read "Progress: " (rpgdm-ironsworn--progresses))))
  (save-excursion
    (rpgdm-ironsworn--mark-progress-track label)))

(provide 'rpgdm-ironsworn)
;;; rpgdm-ironsworn.el ends here
