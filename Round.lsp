;; /* *********************************************************************
;; Function Name: toss-coin
;; Purpose: Simulate a coin toss and return the result.
;; Parameters: None
;; Return Value: 
;;     - 1 if the coin toss results in heads.
;;     - 2 if the coin toss results in tails.
;; Algorithm:
;;     - Generate a random integer between 1 and 2 using the 'random' function.
;;     - Add 1 to the generated value to ensure the result is either 1 or 2.
;;     - Return the result to represent heads (1) or tails (2).
;; Assistance Received: None
;; ********************************************************************* */
(defun toss-coin()
    (+ 1 (random 2))
)


;; /* *********************************************************************
;; Function Name: start-game
;; Purpose: Start a new game of Pente and decide which player goes first through a coin toss.
;; Parameters: None
;; Return Value:
;;     - A list representing the players and their roles in the game:
;;         - (first player) represents the player who goes first.
;;         - (second player) represents the player who goes second.
;; Algorithm:
;;     1. Display a welcome message for starting a new game.
;;     2. Toss a coin to decide which player goes first (1 for heads, 2 for tails).
;;     3. Prompt the user to choose heads (1) or tails (2) and validate the input.
;;     4. Determine the winner of the coin toss:
;;         - If the user's choice matches the toss result, they go first (White).
;;         - If not, the computer goes first (Black).
;;     5. Return a list of the two players and their respective roles.
;; Assistance Received: None
;; ********************************************************************* */
(defun start-game()
    (format t "Starting a new game~%")
    (format t "Tossing a coin to decide who plays first~%")

    (let* ((tossed (toss-coin)))

        (defun get-user-input()
            (princ "Enter 1 for heads or 2 for tails: ")
            (finish-output)
            (let* ((user-input (read-line)))
                (format t "You entered: ~a~%" user-input)
                (handler-case
                    (let ((parsed-input (parse-integer user-input)))
                        (cond
                            ((or (equal parsed-input 1) (equal parsed-input 2))
                                parsed-input)
                            (t
                                (format t "Invalid input. Please enter 1 for heads or 2 for tails.~%")
                                (terpri)
                                (get-user-input))))
                  (error (condition)
                    (format t "Error: ~a~%" condition)
                    (terpri)
                    (get-user-input)))))

        (let* ((user-input (get-user-input)))
            (cond ((equal user-input tossed)
                    (princ "You won the toss.")
                    (terpri)
                    (princ "You will play: White")
                    (terpri)
                    (list 'Human 'Computer))
                
                (t
                    (princ "Computer won the toss.")
                    (terpri)
                    (princ "You will play: Black")
                    (terpri)
                    (list 'Computer 'Human))
            )
        )
    )
)


;; /* *********************************************************************
;; Function Name: load-game
;; Purpose: Load a previously saved game of Pente and restore its state.
;; Parameters: None
;; Return Value:
;;     - A list representing the saved game state with the following elements:
;;         - (first) the game board.
;;         - (second) the player's color.
;;         - (third) the player's type.
;;         - (fourth) the opponent's color.
;;         - (fifth) the opponent's type.
;;         - (sixth) the player's capture.
;;         - (seventh) the opponent's capture.
;;         - (eighth) the human player's score.
;;         - (ninth) the computer player's score.
;;(list board playerColor playerType opponentColor opponentType playerCapture opponentCapture humanScore computerScore)
;; Algorithm:
;;     1. Display a message indicating that a saved game is being loaded.
;;     2. Deserialize the saved game state using the `deserialize` function.
;;     3. Extract the necessary information from the deserialized data:
;;         - Board, player's captures, opponent's captures, human score, computer score, player type, color string, and more.
;;     4. Determine the next player's type, color, and the opponent's type.
;;     5. Return a list containing all the extracted game state elements.
;; Assistance Received: None
;; ********************************************************************* */

(defun load-game()
    (format t "Loading a saved game:~%")

    (let* ((values (deserialize))
       (board (first values))
       (playerCapture (second values))
       (opponentCapture (third values))
       (humanScore (fourth values))
       (computerScore (fifth values))
       (playerType (sixth values))
       (color-string (seventh values))
       (playerColor
        (cond ((string= color-string 'Black) 'B)
              (t 
                (string= color-string 'White) 'W
              )))
       (opponentColor
        (cond ((string= playerColor 'B) 'W)
              
              (t (string= playerColor 'W) 'B)))
        (opponentType
        (cond ((string= playerType 'Human) 'Computer)
              (t 
                (string= playerType 'Computer) 'Human
              )))
       )

      (format t "-----------------------------------------~%")
      (format t "Next player: ~A~%" playerType)
      (format t "Next player color: ~a~%" color-string)
      (format t "-----------------------------------------~%")
 
      (list board playerColor playerType opponentColor opponentType playerCapture opponentCapture humanScore computerScore)
    )
)





  ;; /* *********************************************************************
;; Function Name: play-game
;; Purpose: Manages the gameplay logic of the Pente game between two players.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. Passed by reference.
;; - playerColor (any): The symbol representing the current player's color.
;; - playerType (any): The type of the current player (e.g., human or computer).
;; - opponentColor (any): The symbol representing the opponent's color.
;; - opponentType (any): The type of the opponent (e.g., human or computer).
;; - playerCaptures (integer): The number of captures made by the current player.
;; - opponentCaptures (integer): The number of captures made by the opponent.
;; - moveCount (integer): The current move count.
;; - humanTournament (integer): A number representing the human player's tournament score.
;; - computerTournament (integer): A number representing the computer player's tournament score.
;; Return Value: A list containing the updated board, playerColor, playerType, opponentColor, opponentType, playerCaptures, and moveCount. The return value is for managing the game state.
;; Algorithm:
;; 1. Print the 2D game board.
;; 2. Display information about the current player's turn (name and color).
;; 3. For computer players, prompt to quit and handle quit options.
;; 4. Determine the player's move based on their type (computer or human).
;; 5. Check if the game has ended due to a win or draw.
;; 6. Handle captures (if any).
;; 7. Recursively call `play-game` to continue the game.
;; Assistance Received: None
;; ********************************************************************* */
(defun play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount humanTournament computerTournament)
  (print-2d-board board)

  (format t "--------------------------------------------~%")
  (format t "~a's turn.~%" playerType)
  (format t "~a's color: ~a~%" playerType playerColor)
  (format t "--------------------------------------------~%")
  (cond
    (
      ;if computer's turn
      (string= playerType 'Computer)
      (format t "Do you want to quit? (Enter 'y' to quit)")
      (finish-output)
      (let* ((userInput (read-line)))
        (format t "You entered: ~a~%" userInput)
          (
            cond 
            (
              (string= (string-downcase userInput) "y")
              (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures humanTournament computerTournament)
              
            )
          )
      )

      (let* ((computer-move (computerMove board playerColor moveCount))
              (row (first computer-move))
              (col (second computer-move))
              (new-board (place-stone board row col playerColor)))
        (format t "Computer move: ~a~%" (third computer-move))
        
          (cond
            ((check-five new-board row col playerColor)
            (format t "Five in a row.~%")
            (print-2d-board new-board)
            ;;return the board and everything
            (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 4)
            )
            (t
                (cond
                  ((let* ((captured-data (recursively-check-capture new-board row col playerColor playerCaptures))
                          (captured-board (first captured-data))
                          (next-playerCaptures (second captured-data)))
                    (cond
                      (captured-board
                          (captures 'Computer next-playerCaptures 'Human  opponentCaptures)
                      (cond 
                          ((>= next-playerCaptures 5)
                          (format t "Five captures.~%")
                          (print-2d-board captured-board)
                          (list captured-board playerColor playerType opponentColor opponentType next-playerCaptures opponentCaptures 0)) 
                          
                        
                        (t
                        
                        (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount) humanTournament computerTournament))))
                      (t
                        (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount) humanTournament computerTournament))))
                  ))

                
                ))
      ))
  (
    t 
    (string= playerType 'Human)
    ;get user move
    (let* ((user-move (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount humanTournament computerTournament))
          (row (first user-move))
          (col (second user-move)))
      (let* ((new-board (place-stone board row col playerColor)))
        (cond
            ((check-five new-board row col playerColor)
            (format t "Five in a row.~%")
            (print-2d-board new-board)
            ;;return the board and everything here
            (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 4))
            (t

                (cond
                  ((let* ((captured-data (recursively-check-capture new-board row col playerColor playerCaptures))
                          (captured-board (first captured-data)) 
                          (next-playerCaptures (second captured-data)))
                    (cond
                      (captured-board
                            (captures 'Human next-playerCaptures 'Computer  opponentCaptures)

                        (cond 
                          ((>= next-playerCaptures 5)
                          (format t "Five captures.~%")
                          (print-2d-board captured-board)
                          (list captured-board playerColor playerType opponentColor opponentType next-playerCaptures opponentCaptures 0)) 
                          
                        
                        (t
                          (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount) humanTournament computerTournament))))
                      (t
                        (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount) humanTournament computerTournament))))
                  ))

                ))
      )
    )
  )

)
)

;; /* *********************************************************************
;; Function Name: captures
;; Purpose: Display the current number of captures for both players.
;; Parameters:
;;     - playerType: The type of the current player (e.g., 'Human' or 'Computer').
;;     - playerCapture: The number of captures for the current player.
;;     - opponentType: The type of the opponent player.
;;     - opponentCapture: The number of captures for the opponent player.
;; Return Value: None (void function for display purposes).
;; ********************************************************************* */
(defun captures(playerType playerCapture opponentType opponentCapture)
  (format t "--------------------------------------------~%")
  (format t "~a Captures: ~a~%" playerType playerCapture)
  (format t "~a Captures: ~a~%" opponentType opponentCapture)
  (format t "--------------------------------------------~%"))


;; /* *********************************************************************
;; Function Name: calculate-score
;; Purpose: Calculate and display the scores for both players in the game.
;; Parameters:
;;    - result: A list containing game result data, including the board state, player types, captures, and other information.

;; Return Value: A list of two integers representing the scores of the two players.

;; Algorithm: This function takes a result data list as input, which includes the game board, player types, captures, and other relevant information. It calculates and displays the scores for both players in the game based on the specified rules. The scores are printed to the standard output with proper labeling and formatting.

;; Assistance Received: None.
;; ********************************************************************* */
(defun calculate-score (result)
  (let* ((board (first result))
         (winnerColor (second result))
         (winnerType (third result))
         (opponentColor (fourth result))
         (opponentType (fifth result))
         (winnerCapture (sixth result))
         (opponentCapture (seventh result))
         (isFiveinarow (eighth result )))

    (cond 
      ((equal winnerType 'Human)
        (format t "Human Scores: ~a~%" (+ winnerCapture isFiveinarow (count-four board winnerColor)))
        (format t "Computer Scores: ~a~%" (+ opponentCapture (count-four board opponentColor)))
        (list (+ winnerCapture isFiveinarow (count-four board winnerColor)) (+ opponentCapture (count-four board opponentColor))))
      (t
        (format t "Human Scores: ~a~%" (+ opponentCapture (count-four board opponentColor)))
        (format t "Computer Scores: ~a~%" (+ winnerCapture isFiveinarow (count-four board winnerColor)))
        (list (+ opponentCapture (count-four board opponentColor)) (+ winnerCapture isFiveinarow (count-four board winnerColor)))))
   ))


;; /* *********************************************************************
;; Function Name: start-round
;; Purpose: Start a new round of the game with the specified player types and scores.
;; Parameters:
;;    - firstPlayerType: The type of the first player ('Human' or 'Computer').
;;    - secondPlayerType: The type of the second player ('Human' or 'Computer').
;;    - humanScore: The current score of the human player.
;;    - computerScore: The current score of the computer player.

;; Return Value: A list containing the game result, including the final state of the board and updated scores.

;; Algorithm: This function initializes a 19x19 game board and then proceeds to play a new round of the game with the specified player types and scores. The game result, including the final board state and scores, is returned.

;; Assistance Received: None.
;; ********************************************************************* */
(defun start-round (firstPlayerType secondPlayerType humanScore computerScore)
  (let* ((my-2d-board (make-2d-board 19 19))
         (game-result (play-game my-2d-board 'W firstPlayerType 'B secondPlayerType 0 0 1 humanScore computerScore)))
    (values game-result)))


