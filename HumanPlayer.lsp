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

(defun ask-for-help(board playerColor)
  (format t "Help asked")

)

(defun quit-the-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (format t "Do you want to serialize the game?(y/n)")
  (finish-output)
  (let ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (cond
    (
      (string= userInput "y")
      (serialize board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
    )
    (t
      (exit)
    ))  
  )

)

(defun serialize(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (format t "Enter filename: ~%")
  (finish-output)

  (let* ((filename (read-line))
         (color-string
          (cond
            ((equal playerColor "B") "Black")
            (t "White")))
         (data
          (cond
            ((string= playerType "Human")
             (list board playerType color-string playerCaptures opponentCaptures 0 0))
            (t
             (list board playerType color-string opponentCaptures playerCaptures 0 0))))
         )

    (with-open-file (stream filename
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (print data stream))
    )

  (format t "File saved successfully")
  (finish-output)
  (exit)
)



(defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (format t "Enter the move: ")
  (finish-output)
  (let ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (cond 
      ((string= userInput "help")
        (ask-for-help board playerColor))
      ((string= userInput "quit")
        (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
      )
      (t
        (let* ((converted (convertMove userInput))
               (row (first converted))
               (col (second converted)))
          (cond
            ((and (>= row 0) (< row 19) (>= col 0) (< col 19))
             (format t "Good input.~%")
             (format t "Row: ~a~%" row)
             (format t "Col: ~a~%" col)
             (return-from getUserMove converted)) ; Return the converted move
            (t
             (format t "Invalid input.~%"))))))

    (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures) ; Continue the loop to get a new move if necessary
  ))



;(getUserMove)





