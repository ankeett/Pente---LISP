(load "Board.lsp")

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

;; (defun ask-for-help(board playerColor)
;;   (format t "Help asked")

;; )

;; (defun quit-the-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;   (format t "Do you want to serialize the game?(y/n)")
;;   (finish-output)
;;   (let ((userInput (read-line)))
;;     (format t "You entered: ~a~%" userInput)

;;     (cond
;;     (
;;       (string= userInput "y")
;;       (serialize board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;     )
;;     (t
;;       (exit)
;;     ))  
;;   )

;; )

;; (defun serialize(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
;;   (format t "Enter filename: ~%")
;;   (finish-output)

;;   (let* ((filename (read-line))
;;          (color-string
;;           (cond
;;             ((equal playerColor 'B) 'Black)
;;             (t 'White)))
;;          (data
;;           (cond
;;             ((string= playerType 'Human)
;;              (list board playerCaptures 0 opponentCaptures 0 playerType color-string ))
;;             (t
;;              (list board opponentCaptures 0 playerCaptures  0 playerType color-string ))))
;;          )

;;     (with-open-file (stream filename
;;                         :direction :output
;;                         :if-exists :supersede
;;                         :if-does-not-exist :create)
;;       (print data stream))
;;     )

;;   (format t "File saved successfully")
;;   (finish-output)
;;   (exit)
;; )



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


(defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount)
  (format t "Enter the move: ")
  (finish-output)
  (let* ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (cond 
      ((string= userInput "help")
        (ask-for-help board playerColor))
      ((string= userInput "quit")
        (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures))
      (t
        (let* ((converted (convertMove userInput))
               (row (first converted))
               (col (second converted)))
          (cond
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

    (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount) ; Continue the loop to get a new move if necessary
  ))


(defun recursively-check-capture (board row col playerColor playerCaptures)
  (let* ((captured-board (check-capture board row col playerColor)))
    (cond
      (captured-board
       (recursively-check-capture captured-board row col playerColor (+ 1 playerCaptures)))
      (t
       (list board playerCaptures)))))

(defun is-three-points-away (initial-pos next-pos)
  (let* ((next-pos-upper (string-upcase next-pos)) ; Create a new variable with uppercase next-pos
         (initial-pos-row (parse-integer (subseq initial-pos 1)))
         (initial-pos-col (- (char-code (char initial-pos 0)) (char-code #\A)))
         (next-pos-row (parse-integer (subseq next-pos-upper 1))) ; Use next-pos-upper
         (next-pos-col (- (char-code (char next-pos-upper 0)) (char-code #\A))) ; Use next-pos-upper
         (row-difference (abs (- initial-pos-row next-pos-row)))
         (col-difference (abs (- initial-pos-col next-pos-col)))
         (at-least-three-points-away (or (>= row-difference 3)
                                        (>= col-difference 3)
                                        (and (>= row-difference 3) (>= col-difference 3)))))
    (cond (at-least-three-points-away t)
          (t nil))))



;randomize row and col
;convert into move
;check if the move is three intersection away form "J10"
;if yes, return, if not randomize again

(defun is-empty-cell (board row col)
  (string= (get-board-value board row col) 'O)
  )

(defun random-move()
  (list (random 19) (random 19))
)

;; (let* ((my-2d-board
;;   '((O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O B B O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O))
;;     ))
;;     (print-2d-board my-2d-board)

;;     (print  (evaluate-second-move my-2d-board 'W))

;; )


(defun second-move (board)
  (let* ((row (random 19))
        (col (random 19)))
    (let ((move (convert-to-move row col)))
      (cond
        ((and (is-three-points-away "J10" move) (empty-cell-p board row col))
         (list row col))
        (t
         (second-move board))))))

(defun empty-cell-p (board row col)
  (string= (get-board-value board row col) 'O))

(defun convert-to-move (row col)
  (let* ((move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string (- 19 row)))))
    move))

(defun is-three-points-away (initial-pos next-pos)
  (let* ((next-pos-upper (string-upcase next-pos)) ; Create a new variable with uppercase next-pos
         (initial-pos-row (parse-integer (subseq initial-pos 1)))
         (initial-pos-col (- (char-code (char initial-pos 0)) (char-code #\A)))
         (next-pos-row (parse-integer (subseq next-pos-upper 1))) ; Use next-pos-upper
         (next-pos-col (- (char-code (char next-pos-upper 0)) (char-code #\A))) ; Use next-pos-upper
         (row-difference (abs (- initial-pos-row next-pos-row)))
         (col-difference (abs (- initial-pos-col next-pos-col)))
         (at-least-three-points-away (or (>= row-difference 3)
                                        (>= col-difference 3)
                                        (and (>= row-difference 3) (>= col-difference 3)))))
    (cond (at-least-three-points-away t)
          (t nil))))


(let* ((my-2d-board
  '((O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O B B O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O B B O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O)
    (O O O O O O O O O O O O O O O O O O O))
    ))
    (print-2d-board my-2d-board)

    (print  (second-move my-2d-board))

)