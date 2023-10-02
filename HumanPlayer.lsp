(defun makeMove ()

)


(defun convertMove (move)
  (let* ((colChar (char-upcase (char move 0)))
         (row (- 19 (parse-integer (subseq move 1))))
         (col (- (char-code colChar) (char-code #\A))))
    (list row col)))


(defun convertMove (move)
  (let* ((colChar (char-upcase (char move 0)))
         (row (- 19 (parse-integer (subseq move 1))))
         (col (- (char-code colChar) (char-code #\A))))
    (list row col)))

(defun getUserMove ()
  (format t "Enter the move: ")
  (finish-output)
  (let ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (let* ((converted (convertMove userInput))
           (row (first converted))
           (col (second converted)))
      (cond
        ((and (>= row 0) (< row 19) (>= col 0) (< col 19))
         (format t "Good input.~%")
         (format t "Row: ~a~%" row)
          (format t "Col: ~a~%" col)
         converted)
        (t
         (format t "Invalid input.~%")
         (getUserMove)
         nil))
      )
    )
  )


;(getUserMove)





