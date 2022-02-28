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
