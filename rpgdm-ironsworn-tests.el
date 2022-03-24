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

(ert-deftest rpgdm-ironsworn--asset-stat-alist-test ()
  ;; Using Lisp to `mock' the function to get an stat value:
  (cl-letf (((symbol-function 'rpgdm-ironsworn-character-stat) (lambda (s) 3)))

    (let* ((stats #s(hash-table size 65 test str-or-keys rehash-size 1.5
                                rehash-threshold 0.8125 data
                                (title "Travels of Kannan"
                                       edge 2 heart 1 iron 1 shadow 2 wits 3
                                       health 5 spirit 5 supply 5 momentum 2
                                       asset-mammoth-health 3
                                       asset-invoke-level 3))))
      (should (string= "Mammoth Health: 3  Invoke Level: 3"
                       (rpgdm-ironsworn--asset-stat-show-all stats))))

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
