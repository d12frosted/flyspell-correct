(ert-deftest flyspell-correct-previous-no-mistakes-test ()
  "Test that `flyspell-correct-previous' doesn't correct any word
in a buffer with no mistakes."
  (with-no-mistakes
   (ensure-no-corrections)
   (flyspell-correct-previous (point))))

(ert-deftest flyspell-correct-previous-cursor-beginning-test ()
  "Test that `flyspell-correct-previous' corrects a word when
cursor is at the beginning of the word."
  (with-mistake-in-the-middle|cursor-beginning
   (ensure-correction "versiuns" "versions")
   (flyspell-correct-previous (point))))

(ert-deftest flyspell-correct-previous-cursor-before-test ()
  "Test that `flyspell-correct-previous' doesn't correct any word
when cursor is before the word."
  (with-mistake-in-the-middle|cursor-before
   (ensure-no-corrections)
   (flyspell-correct-previous (point))))

(ert-deftest flyspell-correct-previous-cursor-inside-test ()
  "Test that `flyspell-correct-previous' corrects a word when
cursor is inside of the word."
  (with-mistake-in-the-middle|cursor-inside
   (ensure-correction "versiuns" "versions")
   (flyspell-correct-previous (point))))

(ert-deftest flyspell-correct-previous-cursor-end-test ()
  "Test that `flyspell-correct-previous' corrects a word when
cursor is at the end of the word."
  (with-mistake-in-the-middle|cursor-end
   (ensure-correction "versiuns" "versions")
   (flyspell-correct-previous (point))))

(ert-deftest flyspell-correct-previous-cursor-after-test ()
  "Test that `flyspell-correct-previous' corrects a word when
cursor is after the word."
  (with-mistake-in-the-middle|cursor-after
   (ensure-correction "versiuns" "versions")
   (flyspell-correct-previous (point))))
