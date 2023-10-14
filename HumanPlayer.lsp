(defun convertMove (move)
  (let* ((colChar (char-upcase (char move 0)))
         (row (- 19 (parse-integer (subseq move 1))))
         (col (- (char-code colChar) (char-code #\A))))
    (list row col)))


(defun convert-to-move (row col)
  (let* ((move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string (- 19 row)))))
    move))


(defun ask-for-help (board playerColor)
  (let* ((value  (evaluate-all-cases board playerColor))
         (best-move (convert-to-move (first value) (second value))))
    (format t "Best Move: ~a~%" best-move)
  )
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



;; (defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;   (format t "Enter the move: ")
;;   (finish-output)
;;   (let ((userInput (read-line)))
;;     (format t "You entered: ~a~%" userInput)

;;     (cond 
;;       ((string= userInput "help")
;;         (ask-for-help board playerColor))
;;       ((string= userInput "quit")
;;         (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;       )
;;       (t
;;         (let* ((converted (convertMove userInput))
;;                (row (first converted))
;;                (col (second converted)))
;;           (cond
;;             ((and (>= row 0) (< row 19) (>= col 0) (< col 19))
;;              (format t "Good input.~%")
;;              (format t "Row: ~a~%" row)
;;              (format t "Col: ~a~%" col)
;;              (return-from getUserMove converted)) ; Return the converted move
;;             (t
;;              (format t "Invalid input.~%"))))))

;;     (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures) 
;;   ))


;; (defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount)
;;   (format t "Enter the move: ")
;;   (finish-output)
;;   (let* ((userInput (read-line)))
;;     (format t "You entered: ~a~%" userInput)

;;     (cond 
;;       ((string= userInput "help")
;;         (ask-for-help board playerColor))
;;       ((string= userInput "quit")
;;         (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;       )
;;       (t
;;         (let* ((converted (convertMove userInput))
;;                (row (first converted))
;;                (col (second converted)))
;;           (cond
;;             ((and (>= row 0) (< row 19) (>= col 0) (< col 19))
;;              (cond
;;                ((is-three-points-away "J10" userInput)
;;                 (format t "Good input.~%")
;;                 (format t "Row: ~a~%" row)
;;                 (format t "Col: ~a~%" col)
;;                 (return-from getUserMove converted)) ; Return the converted move
;;                (t
;;                 (format t "Invalid input: Not three points away from J10.~%"))))
;;             (t
;;              (format t "Invalid input.~%"))))))

;;     (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount) ; Continue the loop to get a new move if necessary
;;   ))




;(getUserMove)

;; (let* ((my-2d-board
;;       '((O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O)
;;         (O O O O O O O O O O O O O O O O O O O))
;;         ))
  
;;   ;(getUserMove my-2d-board 'W 'Human 'B 'Computer 0 0 3)


;; )




