(defun convertMove (move)
  (cond
    ((and (stringp move)
          (>= (length move) 2)
          (position (char-upcase (char move 0)) "ABCDEFGHIJKLMNOPQRSTUVWXZ")
          (ignore-errors (<= 1 (parse-integer (subseq move 1))))
          (ignore-errors (<= (parse-integer (subseq move 1)) 19)))
     (let* ((colChar (char-upcase (char move 0)))
            (row (- 19 (parse-integer (subseq move 1))))
            (col (- (char-code colChar) (char-code #\A))))
       (list row col)))
    (t (list nil nil))))



(defun convert-to-move (row col)
  (let* ((move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string (- 19 row)))))
    move))


(defun ask-for-help (board playerColor moveCount)
  (let* ((value  (computerMove board playerColor moveCount))
         (best-move (convert-to-move (first value) (second value))))
    (format t "Best Move: ~a~%" best-move)
  )
)

(defun quit-the-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (format t "Do you want to serialize the game? (Enter 'y' to confirm.)")
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
            ((equal playerColor 'B) 'Black)
            (t 'White)))
         (data
          (cond
            ((string= playerType 'Human)
             (list board playerCaptures 0 opponentCaptures 0 playerType color-string ))
            (t
             (list board opponentCaptures 0 playerCaptures  0 playerType color-string ))))
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

(defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount) 
  (cond
    ((= moveCount 1)
      (format t "Reason: First move always at the center of the board~%")
      (list 9 9)
    )
    (t 
    (format t "Enter the move (e.g., J10) : ~%")
    (format t "Enter HELP for a hint or QUIT for quitting the game.~%")
    (finish-output)
  
  (let* ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (cond 
      ((string= userInput "help")
        (ask-for-help board playerColor moveCount))
      ((string= userInput "quit")
        (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures))
      (t
        (let* ((converted (convertMove userInput))
               (row (first converted))
               (col (second converted)))
          (cond
            ((equal row nil)
              (format t "Invalid input~%")
            )
            (
              (not (is-empty-cell board row col))
              (format t "Spot already taken~%")

            )
            ((and (>= row 0) (< row 19) (>= col 0) (< col 19))
             (cond
               ((= moveCount 3)
                (cond
                  ((is-three-points-away "J10" userInput)
                   (format t "Good input.~%")
                   (format t "Row: ~a~%" row)
                   (format t "Col: ~a~%" col)
                   (return-from getUserMove converted)) ; Return the converted move
                  (t
                   (format t "Invalid input: Not three points away from J10.~%"))))
               (t
                  (format t "Good input.~%")
                  (format t "Row: ~a~%" row)
                  (format t "Col: ~a~%" col)
                  (return-from getUserMove converted)) ; Return the converted move
                
                ))
            (t
             (format t "Invalid input.~%"))))))

    (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount)
  ))  )
  )



