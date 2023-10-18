;; /* *********************************************************************
;; Function Name: computerMove
;; Purpose: Determines the computer's move on the game board based on the current game state.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; - moveCount (integer): The current move count in the game.
;; Return Value: 
;; - A list containing the row, column, and move coordinates of the computer's move.
;; Algorithm:
;; 1. Handles different scenarios based on the move count:
;;    a. If it's the first move, place the stone at the center of the board.
;;    b. If it's the third move, use a specific strategy for the second move.
;;    c. For all other moves, evaluate the game state, make a strategic move, or choose a random move.
;; 2. Returns a list containing the row, column, and move coordinates for the computer's move.
;; Assistance Received: None
;; ********************************************************************* */
(defun computerMove(board player-symbol moveCount)
  (cond
  
    ((equal moveCount 1)
      (format t "--------------------------------------------~%")
      (format t "Reason: First stone always at the center of the board~%")
      (list 9 9 "J10")
    )

    (
      (equal moveCount 3)
      (format t "--------------------------------------------~%")
      (format t "Reason: 3 intersection away from the center of the board~%")
      (second-move board)
    ) 
    (t
      (format t "--------------------------------------------~%")
      (let* ((evaluation-result (evaluate-all-cases board player-symbol))
            (random-value (random-move))
            (row (cond ((equal evaluation-result '(nil nil)) (first random-value))
                        (t (first evaluation-result))))
            (col (cond ((equal evaluation-result '(nil nil)) (second random-value))
                        (t (second evaluation-result))))
            (adjusted-row (- 19 row))
            (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row))))
        (list row col move)))
  )
)

;; /* *********************************************************************
;; Function Name: second-move
;; Purpose: Generate a random valid move for the second turn of the game.
;; Parameters:
;;     - board: The game board represented as a two-dimensional array.
;; Return Value:
;;     - A list containing the generated row, column, and move (in "XY" format).
;;     - If a valid move is generated, it will satisfy the conditions defined below.
;;       Otherwise, the function will recursively attempt to generate a valid move.
;; Algorithm:
;;     1. Generate a random row and column within the board's dimensions (0-18).
;;     2. Convert the row and column into a move in the "A1" format.
;;     3. Check if the generated move is at least three points away from "J10" (the center).
;;     4. Check if the cell at the specified row and column is empty.
;;     5. If both conditions are met, return the generated row, column, and move.
;;     6. If not, recursively call the function to generate another move.
;; Assistance Received: None
;; ********************************************************************* */
(defun second-move (board)
  (let* ((row (random 19))
        (col (random 19)))
    (let ((move (convert-to-move row col)))
      (cond
        ((and (is-three-points-away "J10" move) (empty-cell-p board row col))
         (list row col move))
        (t
         (second-move board))))))



;; /* *********************************************************************
;; Function Name: random-move
;; Purpose: Generates a random move on the game board.
;; Parameters: None.
;; Return Value: 
;; - A list containing random row and column indices.
;; Algorithm:
;; - Uses the 'random' function to generate random row and column indices between 0 and 18.
;; - Returns a list containing these random indices.
;; Assistance Received: None
;; ********************************************************************* */
(defun random-move()
  (list (random 19) (random 19))
)


;; /* *********************************************************************
;; Function Name: empty-cell-p
;; Purpose: Checks if a cell on the game board is empty (contains 'O').
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the cell to be checked.
;; - col (integer): The column index of the cell to be checked.
;; Return Value: 
;; - true if the specified cell is empty (contains 'O'), otherwise false.
;; Algorithm:
;; - Uses the 'get-board-value' function to get the value of the cell.
;; - Checks if the value is equal to 'O' to determine if the cell is empty.
;; - Returns true if the cell is empty, otherwise false.
;; Assistance Received: None
;; ********************************************************************* */
(defun empty-cell-p (board row col)
  (string= (get-board-value board row col) 'O))


;; /* *********************************************************************
;; Function Name: find-winning-move
;; Purpose: Finds a winning move for the player on the game board.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. 
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of a winning move for the player, or '(nil nil)' if no winning move is found.
;; Algorithm:
;; 1. Defines two nested functions, 'check-next-cell' and 'check-cell', for checking cells in a row.
;; 2. 'check-next-cell' checks the next cell in the row, and if the end of the row is reached, it moves to the next row.
;; 3. 'check-cell' checks if the current cell is empty. If it is, it simulates placing the player's symbol and checks if it leads to a winning move.
;; 4. Returns the row and column of the winning move if found, or calls 'check-next-cell' to check the next cell.
;; 5. Initiates the search by calling 'check-cell' with the first cell.
;; 6. Returns a list containing the row and column of a winning move or '(nil nil)' if no winning move is found.
;; Assistance Received: None
;; ********************************************************************* */
(defun find-winning-move (board player-symbol)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) ; Return nil values if no winning move is found
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-five new-board row col player-symbol)
           
            (list row col)) ; Return the row and col of the winning move
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)

;; /* *********************************************************************
;; Function Name: defend-winning-move
;; Purpose: Determines a move to defend against the opponent's potential winning move.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. 
;; - player-symbol (any): The symbol representing the player
;; Return Value: 
;; - A list containing the row and column indices of a move to defend against the opponent's potential winning move.
;; Algorithm:
;; 1. Determines the opponent's symbol based on the player's symbol.
;; 2. Calls the 'find-winning-move' function for the opponent's symbol to check for a potential winning move.
;; 3. Returns a list containing the row and column of a move to block the opponent's potential winning move or '(nil nil)' if no such move is found.
;; Assistance Received: None
;; ********************************************************************* */
(defun defend-winning-move(board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
         )
       )

  ))
    (find-winning-move board opponent-symbol)
  )
)

;; /* *********************************************************************
;; Function Name: find-capture-position
;; Purpose: Determines a position for capturing the maximum number of opponent stones.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of a position to capture the maximum number of opponent stones.
;; Algorithm:
;; 1. Defines two nested functions, 'check-next-cell' and 'check-cell', for checking cells in a row.
;; 2. 'check-next-cell' checks the next cell in the row, and if the end of the row is reached, it moves to the next row.
;; 3. 'check-cell' checks if the current cell is empty. If it is, it simulates placing the player's symbol and checks for potential captures.
;; 4. Recursively checks for potential captures by calling 'recursively-check-capture' and tracks the maximum captures found.
;; 5. Returns the row and column of the position that maximizes captures.
;; 6. Initiates the search by calling 'check-cell' with the first cell.
;; 7. Returns a list containing the row and column indices for capturing the maximum number of opponent stones.
;; Assistance Received: None
;; ********************************************************************* */
(defun find-capture-position (board player-symbol)
  (defun check-next-cell (current-board current-row current-col maxCaptures maxRow maxCol)
    (cond
      ((>= current-row 18)
       (list maxRow maxCol))
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0 maxCaptures maxRow maxCol))
      (t
       (check-cell current-board current-row (+ 1 current-col) maxCaptures maxRow maxCol))))

  (defun check-cell (current-board row col maxCaptures maxRow maxCol)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
            ((let* ((captured-board (recursively-check-capture new-board row col player-symbol 0)))
                  
              (cond
                ((first captured-board)
                  (cond (
                     (> (second captured-board) maxCaptures)
                     (check-next-cell current-board row col (second captured-board) row col)
                  )
                  (t
                    (check-next-cell current-board row col maxCaptures maxRow maxCol)
                  )
                  
                  )
                )
                (t
                  (check-next-cell current-board row col maxCaptures maxRow maxCol)
                )
              ))
            ))
            
            ))
      (t
       (check-next-cell current-board row col maxCaptures maxRow maxCol))))

  (check-cell board 0 0 0 nil nil)

)


;; /* *********************************************************************
;; Function Name: defend-capture-position
;; Purpose: Determines a position to defend against the opponent's potential captures.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of a position to defend against the opponent's potential captures.
;; Algorithm:
;; 1. Determines the opponent's symbol based on the player's symbol.
;; 2. Calls the 'find-capture-position' function for the opponent's symbol to check for a potential capture position.
;; 3. Returns a list containing the row and column indices of a position to defend against the opponent's potential captures.
;; Assistance Received: None
;; ********************************************************************* */
(defun defend-capture-position (board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
         )
       )

  ))
    (find-capture-position board opponent-symbol)
  )

)

;; /* *********************************************************************
;; Function Name: make-four-move
;; Purpose: Determines a move to create a position with four consecutive stones for the player.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of a move to create a position with four consecutive stones for the player, or '(nil nil)' if no such position is found.
;; Algorithm:
;; 1. Defines two nested functions, 'check-next-cell' and 'check-cell', for checking cells in a row.
;; 2. 'check-next-cell' checks the next cell in the row, and if the end of the row is reached, it moves to the next row.
;; 3. 'check-cell' checks if the current cell is empty. If it is, it simulates placing the player's symbol and checks for four consecutive stones.
;; 4. Returns the row and column of the position that creates four consecutive stones if found, or calls 'check-next-cell' to check the next cell.
;; 5. Initiates the search by calling 'check-cell' with the first cell.
;; 6. Returns a list containing the row and column of the move to create four consecutive stones or '(nil nil)' if no such position is found.
;; Assistance Received: None
;; ********************************************************************* */
(defun make-four-move (board player-symbol)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) 
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-four new-board row col player-symbol)
            (list row col))
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)

;; /* *********************************************************************
;; Function Name: defend-four-move
;; Purpose: Determines a move to defend against the opponent's potential position with four consecutive stones.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of a move to defend against the opponent's potential position with four consecutive stones, or '(nil nil)' if no such move is found.
;; Algorithm:
;; 1. Determines the opponent's symbol based on the player's symbol.
;; 2. Calls the 'make-four-move' function for the opponent's symbol to check for a potential position with four consecutive stones.
;; 3. Returns a list containing the row and column indices of a move to defend against the opponent's potential position or '(nil nil)' if no such move is found.
;; Assistance Received: None
;; ********************************************************************* */
(defun defend-four-move(board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
         )
       )

  ))
    (make-four-move board opponent-symbol)
  )
)

;; /* *********************************************************************
;; Function Name: make-consecutive-move
;; Purpose: Determines a move to create a position with a specified number of consecutive stones for the player.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; - consecutive (integer): The number of consecutive stones to create.
;; Return Value: 
;; - A list containing the row and column indices of a move to create a position with the specified number of consecutive stones for the player, or '(nil nil)' if no such position is found.
;; Algorithm:
;; 1. Defines two nested functions, 'check-next-cell' and 'check-cell', for checking cells in a row.
;; 2. 'check-next-cell' checks the next cell in the row, and if the end of the row is reached, it moves to the next row.
;; 3. 'check-cell' checks if the current cell is empty. If it is, it simulates placing the player's symbol and checks for the specified number of consecutive stones.
;; 4. Returns the row and column of the position that creates the specified number of consecutive stones if found, or calls 'check-next-cell' to check the next cell.
;; 5. Initiates the search by calling 'check-cell' with the first cell.
;; 6. Returns a list containing the row and column of the move to create the specified number of consecutive stones or '(nil nil)' if no such position is found.
;; Assistance Received: None
;; ********************************************************************* */
(defun make-consecutive-move (board player-symbol consecutive)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) 
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-consecutive new-board row col player-symbol consecutive)
            (list row col))
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)

;; /* *********************************************************************
;; Function Name: evaluate-all-cases
;; Purpose: Evaluates various move possibilities and returns a move based on certain criteria.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - player-symbol (any): The symbol representing the player.
;; Return Value: 
;; - A list containing the row and column indices of the selected move based on certain criteria.
;; Algorithm:
;; 1. Calls several helper functions to evaluate different move possibilities.
;; 2. Checks for the following move criteria in order:
;;    - Winning Move
;;    - Defending Win
;;    - Defending Four
;;    - Capturing Opponent
;;    - Defending Capture
;;    - Making Four
;;    - Max Consecutive of 3
;;    - Max Consecutive of 2
;;    - Random Move
;; 3. If any of the above criteria is met, it returns the corresponding move with a message indicating the reason.
;; 4. If no criteria are met, it returns a random move.
;; Assistance Received: None
;; ********************************************************************* */
(defun evaluate-all-cases (board player-symbol)
  (let* ((winning-move (find-winning-move board player-symbol))
         (defending-win (defend-winning-move board player-symbol))
         (make-four (make-four-move board player-symbol))
         (defending-four (defend-four-move board player-symbol))
         (capturing-opponent (find-capture-position board player-symbol))
         (defending-capture (defend-capture-position board player-symbol))
         (max-consecutive-pos3 (make-consecutive-move board player-symbol 3))
         (max-consecutive-pos2 (make-consecutive-move board player-symbol 2))
         (random-move (random-move)))
    (cond
      ((not (equal (car winning-move) nil))
       (format t "Reason: Winning Move~%")
       winning-move)
      ((not (equal (car defending-win) nil))
       (format t "Reason: Defending Win~%")
       defending-win)
      ((not (equal (car defending-four) nil))
       (format t "Reason: Defending Four~%")
       defending-four)
      ((not (equal (car capturing-opponent) nil))
       (format t "Reason: Capturing Opponent~%")
       capturing-opponent)
      ((not (equal (car defending-capture) nil))
       (format t "Reason: Defending Capture~%")
       defending-capture)
      ((not (equal (car make-four) nil))
       (format t "Reason: Making Four~%")
       make-four)
      ((not (equal (car max-consecutive-pos3) nil))
       (format t "Reason: Max Consecutive of 3~%")
       max-consecutive-pos3)
      ((not (equal (car max-consecutive-pos2) nil))
       (format t "Reason: Max Consecutive of 2~%")
       max-consecutive-pos2)
      (t
       (format t "Reason: Random Move~%")
       random-move))))
