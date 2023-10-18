;; /* *********************************************************************
;; Function Name: convertMove
;; Purpose: Converts a move represented as a string (e.g., "A1") into row and column indices.
;; Parameters:
;; - move (string): A string representing a move, where the first character is a letter ('A' to 'T' or 'W') and the remaining characters are digits ('1' to '19').
;; Return Value: 
;; - A list containing the row and column indices, or '(nil nil)' if the move is invalid.
;; Algorithm:
;; 1. Checks if the input move is a valid string with a correct format.
;; 2. Extracts the column character and converts it to a corresponding column index (0 to 18).
;; 3. Converts the row number to a row index (0 to 18) by subtracting it from 19.
;; 4. Returns a list containing the row and column indices, or '(nil nil)' if the move is invalid.
;; Assistance Received: None
;; ********************************************************************* */
(defun convertMove (move)
  (cond
    ((and (stringp move)
          (>= (length move) 2)
          (position (char-upcase (char move 0)) "ABCDEFGHIJKLMNOPQRS")
          (ignore-errors (<= 1 (parse-integer (subseq move 1))))
          (ignore-errors (<= (parse-integer (subseq move 1)) 19)))
     (let* ((colChar (char-upcase (char move 0)))
            (row (- 19 (parse-integer (subseq move 1))))
            (col (- (char-code colChar) (char-code #\A))))
       (list row col)))
    (t (list nil nil))))


;; /* *********************************************************************
;; Function Name: convert-to-move
;; Purpose: Converts row and column indices into a move string (e.g., "A1").
;; Parameters:
;; - row (integer): The row index (0 to 18).
;; - col (integer): The column index (0 to 18).
;; Return Value: 
;; - A string representing the move in the format "A1" 
;; Algorithm:
;; 1. Converts the column index to a corresponding character.
;; 3. Converts the row index to the row number by subtracting it from 19.
;; 4. Concatenates the column character and row number to create the move string.
;; 5. Returns the move string
;; Assistance Received: None
;; ********************************************************************* */
(defun convert-to-move (row col)
  (let* ((move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string (- 19 row)))))
    move))


;; /* *********************************************************************
;; Function Name: ask-for-help
;; Purpose: Asks for computer assistance to determine the best move for a player.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - playerColor (any): The symbol representing the player.
;; - moveCount (integer): The number of moves played in the game.
;; Return Value: None (void function), but it prints the best move to the console.
;; Algorithm:
;; 1. Calls the `computerMove` function to obtain the best move for the player.
;; 2. Converts the row and column indices of the best move to a move string.
;; 3. Prints the best move to the console.
;; Assistance Received: None
;; ********************************************************************* */
(defun ask-for-help (board playerColor moveCount)
  (let* ((value  (computerMove board playerColor moveCount))
         (best-move (convert-to-move (first value) (second value))))
    (format t "Best Move: ~a~%" best-move)
  )
)

;; /* *********************************************************************
;; Function Name: quit-the-game
;; Purpose: Allows the player to quit the game and potentially serialize the game state.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - playerColor (any): The symbol representing the player's color.
;; - playerType (any): The type of the player (e.g., human or computer).
;; - opponentColor (any): The symbol representing the opponent's color (typically a character or value).
;; - opponentType (any): The type of the opponent (e.g., human or computer).
;; - playerCaptures (integer): The number of captures made by the player.
;; - opponentCaptures (integer): The number of captures made by the opponent.
;; - humanTournament (integer): A number representing the human player's tournament score.
;; - computerTournament (integer): A number representing the computer player's tournament score.
;; Return Value: None (void function). Depending on the user's choice, it may serialize the game state or exit the game.
;; Algorithm:
;; 1. Prompts the player if they want to serialize the game by entering 'y.'
;; 2. Reads the user's input.
;; 3. If the input is 'y,' it calls the `serialize` function with the game state and tournament progress.
;; 4. If the input is not 'y,' it exits the game.
;; Assistance Received: None
;; ********************************************************************* */
(defun quit-the-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures humanTournament computerTournament)
  (format t "Do you want to serialize the game? (Enter 'y' to confirm!)~%")
  (finish-output)
  (let ((userInput (read-line)))
    (format t "You entered: ~a~%" userInput)

    (cond
    (
      (string= (string-downcase userInput) "y")
      (serialize board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures humanTournament computerTournament)
    )
    (t
      (exit)
    ))  
  )

)

;; /* *********************************************************************
;; Function Name: getUserMove
;; Purpose: Gets a user's move in the game and handles various options.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - playerColor (any): The symbol representing the player's color.
;; - playerType (any): The type of the player (e.g., human or computer).
;; - opponentColor (any): The symbol representing the opponent's color.
;; - opponentType (any): The type of the opponent (e.g., human or computer).
;; - playerCaptures (integer): The number of captures made by the player.
;; - opponentCaptures (integer): The number of captures made by the opponent.
;; - moveCount (integer): The current move count.
;; - humanTournament (integer): A number representing the human player's tournament score.
;; - computerTournament (integer): A number representing the computer player's tournament score.
;; Return Value: A list containing the row and column of the user's move.
;; Algorithm:
;; 1. If it's the first move (moveCount = 1), explain that the first move should be at the center of the board and return (9, 9).
;; 2. For subsequent moves:
;;    a. Prompt the user to enter their move in the format "J10" and give options to enter "HELP" for a hint or "QUIT" to quit the game.
;;    b. Read the user's input.
;;    c. Handle "HELP" and "QUIT" commands appropriately.
;;    d. Attempt to convert the user's input into a valid move.
;;    e. Check if the move is within bounds, not already occupied, and follows specific criteria based on the moveCount.
;;    f. If the move is valid, return the row and column of the user's move. If not, provide feedback and re-prompt for a move.
;; Assistance Received: None
;; ********************************************************************* */
(defun getUserMove (board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount humanTournament computerTournament) 
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
      ((string= (string-downcase userInput) "help")
        (ask-for-help board playerColor moveCount))
      ((string= (string-downcase userInput) "quit")
        (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures humanTournament computerTournament))
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
                   (return-from getUserMove converted)) ; Return the converted move
                  (t
                   (format t "Invalid input: Not three points away from J10.~%"))))
               (t
                  (return-from getUserMove converted)) ; Return the converted move
                ))
            (t
             (format t "Invalid input.~%"))))))

    (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount humanTournament computerTournament)
  )))
)

;; /* *********************************************************************
;; Function Name: is-three-points-away
;; Purpose: Determine if the next position is at least three points away from the initial position.
;; Parameters:
;;     - initial-pos: A string representing the initial position (e.g., "J10").
;;     - next-pos: A string representing the next position to be checked.
;; Return Value:
;;     - Returns true if the next position is at least three points away from the initial position; otherwise, returns false.
;; Algorithm:
;;     1. Convert the initial and next positions to row and column coordinates.
;;     2. Calculate the absolute differences in rows and columns.
;;     3. Check if at least one of the differences is greater than or equal to 3, indicating that the positions are three points away.
;;     4. Return true if the condition is met, otherwise return false.
;; Assistance Received: None
;; ********************************************************************* */
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
