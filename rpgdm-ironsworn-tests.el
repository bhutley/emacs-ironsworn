(ert-deftest rpgdm-ironsworn--results-test ()
  (should (equal (rpgdm-ironsworn--results 3 2 4 1)
                 "Strong hit :: 5 (3 + 2) → 4 / 1"))
  (should (equal (rpgdm-ironsworn--results 3 2 8 1)
                 "Weak hit :: 5 (3 + 2) → 8 / 1"))
  (should (equal (rpgdm-ironsworn--results 3 2 8 6)
                 "Miss :: 5 (3 + 2) → 8 / 6"))
  (should (equal (rpgdm-ironsworn--results 3 2 6 6)
                 "Miss :: 5 (3 + 2) → 6 / 6 ← Create a Twist"))

  (should (equal (rpgdm-ironsworn--results 3 2 8 6 7)
                 "Miss :: 5 (3 + 2) → 8 / 6 -- Burn momentum for a Weak hit"))
  (should (equal (rpgdm-ironsworn--results 3 2 8 6 9)
                 "Miss :: 5 (3 + 2) → 8 / 6 -- Burn momentum for a Strong hit")))

(ert-deftest rpgdm-ironsworn--good-character-assets-test ()
  (should (rpgdm-ironsworn--good-character-assets     '("foo" "bar" "baz")))
  (should (rpgdm-ironsworn--good-character-assets     '(("Companions :: Dog" . "assets/companions/dog.org")
                                                        ("Paths :: Good Guy" . "assets/paths/good-guy.org")
                                                        ("Ritual :: Booboo" . "assets/ritual/booboo.org"))))
  (should-not (rpgdm-ironsworn--good-character-assets '("foo" "bar" "paths/shadow-kin.org")))
  (should-not (rpgdm-ironsworn--good-character-assets '("foo" "bar" "foo")))
  (should-not (rpgdm-ironsworn--good-character-assets '("assets/companions/dog.org"
                                                        "assets/paths/good-guy.org"
                                                        "assets/companions/monkey.org")))
  (should-not (rpgdm-ironsworn--good-character-assets '(("Companions :: Dog" . "assets/companions/dog.org")
                                                        ("Paths :: Good Guy" . "assets/paths/good-guy.org")
                                                        ("Companions :: Monkey" . "assets/companions/monkey.org")))))

(ert-deftest rpgdm-ironsworn--some-character-assets-test ()
  (should (= 4 (seq-length (rpgdm-ironsworn--some-character-assets '(1 2 3 4 5 6) 4))))
  (should (= 3 (seq-length (rpgdm-ironsworn--some-character-assets '(1 2 3 4 5 6))))))

(ert-deftest rpgdm-ironsworn--read-stat-test ()
  ;; Numbers with a minus sign always should indicate a decrease to the current value:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "-2")))
    (should (equal (rpgdm-ironsworn--read-stat 'health) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'spirit) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'supply) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:decrease 2))))

  ;; Numbers with a minus sign always should indicate a increase to the current value:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "+2")))
    (should (equal (rpgdm-ironsworn--read-stat 'health) '(:increase 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'spirit) '(:increase 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'supply) '(:increase 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:increase 2))))

  ;; Numbers with a minus sign always should indicate a new setting:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "=2")))
    (should (equal (rpgdm-ironsworn--read-stat 'health) '(:absolute 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'spirit) '(:absolute 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'supply) '(:absolute 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:absolute 2))))

  ;; Just a number should change based on the type so stat, most go down, momentum goes up:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "2")))
    (should (equal (rpgdm-ironsworn--read-stat 'health) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'spirit) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'supply) '(:decrease 2)))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:increase 2))))

  ;; No numeric value, most stats go down by one, but momentum goes up by one:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "")))
    (should (equal (rpgdm-ironsworn--read-stat 'health) '(:decrease 1)))
    (should (equal (rpgdm-ironsworn--read-stat 'spirit) '(:decrease 1)))
    (should (equal (rpgdm-ironsworn--read-stat 'supply) '(:decrease 1)))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:increase 1))))

  ;; Anything else should return a :reset, as it will take the default value:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "go back")))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:reset 0)))))

(ert-deftest rpgdm-ironsworn--asset-stat-key-test ()
  (should (symbolp (rpgdm-ironsworn--asset-stat-key "Foo bar")))
  (should (string= 'asset-foo-bar (rpgdm-ironsworn--asset-stat-key "Foo bar")))
  (should (string= 'asset-foos-bar (rpgdm-ironsworn--asset-stat-key "Foo's bar"))))

(ert-deftest rpgdm-ironsworn--asset-stat-name-test ()
  (should (string= "Foo Bar" (rpgdm-ironsworn--asset-stat-name 'asset-foo-bar))))

(ert-deftest rpgdm-ironsworn--asset-stat-alist-test ()
  (let* ((stats #s(hash-table size 65 test str-or-keys rehash-size 1.5 rehash-threshold 0.8125 data
                              (title "Travels of Kannan"
                                     edge 2 heart 1 iron 1 shadow 2 wits 3
                                     health 5 spirit 5 supply 5 momentum 2
                                     asset-mammoth-health 5
                                     asset-invoke-level 3)))
         (assets (rpgdm-ironsworn--asset-stat-alist stats)))
    (should (= (length assets) 2))
    (should (eq (alist-get "Mammoth Health" assets 0 nil 'equal) 'asset-mammoth-health))
    (should (eq (alist-get "Invoke Level" assets 0 nil 'equal) 'asset-invoke-level))))

(ert-deftest rpgdm-ironsworn--asset-stat-show-all-test ()
  ;; Using Lisp to `mock' the function to get an stat value:
  (cl-letf (((symbol-function 'rpgdm-ironsworn-character-stat) (lambda (s) 3)))

    (let* ((stats #s(hash-table size 65 test str-or-keys rehash-size 1.5
                                rehash-threshold 0.8125 data
                                (title "Travels of Kannan"
                                       edge 2 heart 1 iron 1 shadow 2 wits 3
                                       health 5 spirit 5 supply 5 momentum 2
                                       asset-mammoth-health 3
                                       asset-invoke-level 3))))
      ;; Hash table key order isn't guaranteed, so compare sorted parts:
      (should (equal '("Invoke Level: 3" "Mammoth Health: 3")
                     (sort (s-split "  " (rpgdm-ironsworn--asset-stat-show-all stats))
                           #'string<))))

    ;; Return an empty string if there are not asset-related stats:
    (let* ((stats #s(hash-table size 65 test str-or-keys rehash-size 1.5
                                rehash-threshold 0.8125 data
                                (title "Travels of Kannan"
                                       edge 2 heart 1 iron 1 shadow 2 wits 3
                                       health 5 spirit 5 supply 5 momentum 2))))
      (should (string= "" (rpgdm-ironsworn--asset-stat-show-all stats))))))

(ert-deftest rpgdm-ironsworn--move-tuple-test ()
  (let ((file "moves/fate/ask-the-oracle.org")
        (full "~/other/over/here/moves/fate/ask-the-oracle.org"))
    (should (equal (list "Fate :: ask the oracle" file)
                   (rpgdm-ironsworn--move-tuple file)))
    (should (equal (list "Fate :: ask the oracle" full)
                   (rpgdm-ironsworn--move-tuple full)))))

(ert-deftest rpgdm-ironsworn-progress-level-label-test ()
  (should (equal (rpgdm-ironsworn-progress-level-label 1) "epic"))
  (should (equal (rpgdm-ironsworn-progress-level-label 12) "troublesome"))
  (should (equal (rpgdm-ironsworn-progress-level-label 4) "formidable")))

(ert-deftest rpgdm-ironsworn--progress-box-test ()
  (should (equal (rpgdm-ironsworn--progress-box 0 0)  "|   |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 0 1)  "| - |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 0 2)  "| x |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 0 3)  "| * |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 1 0)  "| ■ |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 2 0)  "| ■ | ■ |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 3 0)  "| ■ | ■ | ■ |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 0)  "| ■ | ■ | ■ | ■ |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 1)  "| ■ | ■ | ■ | ■ | - |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 2)  "| ■ | ■ | ■ | ■ | x |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 3)  "| ■ | ■ | ■ | ■ | * |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 5 0)  "| ■ | ■ | ■ | ■ | ■ |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 10 0) "| ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |"))
  ;; Negative test cases
  (should (equal (rpgdm-ironsworn--progress-box 11 0) "| ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |"))
  (should (equal (rpgdm-ironsworn--progress-box -1 0) "|   |   |   |   |   |   |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 8)  "| ■ | ■ | ■ | ■ | ■ | ■ |   |   |   |   |"))
  (should (equal (rpgdm-ironsworn--progress-box 4 6)  "| ■ | ■ | ■ | ■ | ■ | x |   |   |   |   |")))

(ert-deftest rpgdm-ironsworn--progress-to-str-test ()
  (should (equal (rpgdm-ironsworn--progress-to-str :IRONSWORN-PROGRESS-EPIC)
                 "ironsworn-progress-epic")))

(ert-deftest rpgdm-ironsworn--read-stat-negative-test ()
  ;; An absolute value can be negative, e.g. momentum after a setback:
  (cl-letf (((symbol-function 'read-string) (lambda (s) "=-3")))
    (should (equal (rpgdm-ironsworn--read-stat 'momentum) '(:absolute -3)))))

(ert-deftest rpgdm-ironsworn-current-character-state-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n"
            ":PROPERTIES:\n:ironsworn-edge: 2\n:ironsworn-momentum: -2\n:END:\n"
            "** Into the Barrow\n"
            ":PROPERTIES:\n:ironsworn-edge: 3\n:END:\n"
            "Some notes.\n")
    (let ((state (rpgdm-ironsworn-current-character-state)))
      (should (equal (gethash 'title state) "Into the Barrow"))
      ;; Lower headings take precedence over higher ones:
      (should (= (gethash 'edge state) 3))
      ;; Negative values are numbers, not strings:
      (should (= (gethash 'momentum state) -2)))))

(ert-deftest rpgdm-ironsworn-progress-mark-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n")
    (rpgdm-ironsworn-progress-create "Slay the Wyrm" 8)
    (should (= (rpgdm-ironsworn-progress-amount "Slay the Wyrm") 0))
    (rpgdm-ironsworn-progress-mark "Slay the Wyrm")
    (should (= (rpgdm-ironsworn-progress-amount "Slay the Wyrm") 2))
    (rpgdm-ironsworn-progress-mark "Slay the Wyrm" 2)
    (should (= (rpgdm-ironsworn-progress-amount "Slay the Wyrm") 6))))

(ert-deftest rpgdm-ironsworn--oracle-result-test ()
  ;; The threshold itself answers yes:
  (should (equal (rpgdm-ironsworn--oracle-result ?c 12) "Yes"))
  (should (equal (rpgdm-ironsworn--oracle-result ?c 10) "No"))
  (should (equal (rpgdm-ironsworn--oracle-result ?h 51) "Yes"))
  (should (equal (rpgdm-ironsworn--oracle-result ?h 50) "No"))
  (should (equal (rpgdm-ironsworn--oracle-result ?n 91) "Yes"))
  (should (equal (rpgdm-ironsworn--oracle-result ?n 90) "No"))
  ;; Matches give an extreme answer, whether yes or no:
  (should (equal (rpgdm-ironsworn--oracle-result ?c 11) "Extreme Yes, or a twist"))
  (should (equal (rpgdm-ironsworn--oracle-result ?h 44) "Extreme No, or a twist"))
  (should (equal (rpgdm-ironsworn--oracle-result ?n 100) "Extreme Yes, or a twist")))

(ert-deftest rpgdm-ironsworn--clamp-stat-test ()
  (should (= (rpgdm-ironsworn--clamp-stat 'health 7) 5))
  (should (= (rpgdm-ironsworn--clamp-stat 'health -2) 0))
  (should (= (rpgdm-ironsworn--clamp-stat 'momentum 12) 10))
  (should (= (rpgdm-ironsworn--clamp-stat 'momentum -8) -6))
  (should (= (rpgdm-ironsworn--clamp-stat 'momentum -3) -3))
  ;; Asset stats aren't limited:
  (should (= (rpgdm-ironsworn--clamp-stat 'asset-mammoth-health 9) 9)))

(ert-deftest rpgdm-ironsworn-adjust-stat-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n:PROPERTIES:\n:ironsworn-health: 4\n:END:\n")
    (cl-letf (((symbol-function 'read-string) (lambda (s) "+3")))
      (rpgdm-ironsworn-adjust-stat 'health 5))
    (should (= (rpgdm-ironsworn-character-stat 'health) 5))
    ;; Junk input with no default leaves the stat alone:
    (cl-letf (((symbol-function 'read-string) (lambda (s) "junk")))
      (rpgdm-ironsworn-adjust-stat 'health))
    (should (= (rpgdm-ironsworn-character-stat 'health) 5))))

(ert-deftest rpgdm-ironsworn-progress-cap-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n")
    (rpgdm-ironsworn-progress-create "Escape the Barrow" 12)
    (rpgdm-ironsworn-progress-mark "Escape the Barrow" 5)
    (should (= (rpgdm-ironsworn-progress-amount "Escape the Barrow") 10))
    (should (equal (caddr (assoc "Escape the Barrow" (rpgdm-ironsworn-character-progresses)))
                   40))))

(ert-deftest rpgdm-ironsworn-nested-progress-test ()
  ;; Tracks on an ancestor heading are found, marked and deleted from a subheading,
  ;; and a file with no level-1 headings works too:
  (with-temp-buffer
    (org-mode)
    (insert "** Travels of Kannan\n")
    (rpgdm-ironsworn-progress-create "Slay the Wyrm" 8)
    (goto-char (point-max))
    (insert "*** Into the Barrow\nNotes here.\n")
    (should (= (gethash 'momentum (rpgdm-ironsworn-current-character-state) 0) 0))
    (rpgdm-ironsworn-progress-mark "Slay the Wyrm")
    (should (= (rpgdm-ironsworn-progress-amount "Slay the Wyrm") 2))
    (rpgdm-ironsworn-progress-delete "Slay the Wyrm")
    (should-not (rpgdm-ironsworn-character-progresses))
    (should-not (string-match-p "ironsworn-progress" (buffer-string)))))

(ert-deftest rpgdm-ironsworn--table-key-test ()
  (should (equal (rpgdm-ironsworn--table-key "Sea Cave") "sea-cave"))
  (should (equal (rpgdm-ironsworn--table-key "Frozen Cavern") "frozen-cavern"))
  (should (equal (rpgdm-ironsworn--table-key "barrow") "barrow")))

(ert-deftest rpgdm-ironsworn-oracle-site-name-test ()
  ;; Multi-word domains, and domains without a place table, still get a place:
  (dolist (domain '("Sea Cave" "Frozen Cavern" "Barrow"))
    (should-not (string-match-p "nil" (rpgdm-ironsworn-oracle-site-name domain)))))

(ert-deftest rpgdm-ironsworn--make-move-test ()
  ;; Moves without a move-stats property, like Pay the Price, roll nothing:
  (should-not (rpgdm-ironsworn--make-move nil)))

(ert-deftest rpgdm-ironsworn-discover-a-site-test ()
  ;; Each site keeps its own theme and domain on its own heading:
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n")
    (rpgdm-ironsworn-discover-a-site "Haunted" "Sea Cave" "Drowned Hollow" 8)
    (goto-char (point-max))
    (rpgdm-ironsworn-discover-a-site "Ancient" "Barrow" "Kael's Rest" 4)
    (goto-char (point-max))
    (let ((state (rpgdm-ironsworn-current-character-state)))
      (should (equal (gethash 'site-theme state) "ancient"))
      (should (equal (gethash 'site-domain state) "barrow")))
    (goto-char (point-min))
    (search-forward "Drowned Hollow")
    (let ((state (rpgdm-ironsworn-current-character-state)))
      (should (equal (gethash 'site-theme state) "haunted"))
      (should (equal (gethash 'site-domain state) "sea-cave")))
    ;; The character's own heading has no site data:
    (goto-char (point-min))
    (should-not (gethash 'site-theme (rpgdm-ironsworn-current-character-state)))))

(ert-deftest rpgdm-ironsworn--reveal-a-danger-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* Travels of Kannan\n")
    ;; Without a site, a theme or domain danger is returned as is:
    (cl-letf (((symbol-function 'rpgdm-tables-choose) (lambda (_) "Check the theme card.")))
      (should (equal (rpgdm-ironsworn--reveal-a-danger) "Check the theme card.")))
    (rpgdm-ironsworn-discover-a-site "Haunted" "Sea Cave" "Drowned Hollow" 8)
    ;; With a site, the theme's table is used:
    (let ((tables-chosen nil))
      (cl-letf (((symbol-function 'rpgdm-tables-choose)
                 (lambda (table) (push table tables-chosen) "Check the domain card.")))
        (rpgdm-ironsworn--reveal-a-danger))
      (should (member "danger/domain/sea-cave" tables-chosen)))))
