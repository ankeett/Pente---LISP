(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")
(load "test.lsp")

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



;; /* *********************************************************************
;; Function Name: tournament
;; Purpose: Conducts a tournament between a human player and a computer player and calculates the final scores.
;;    The function starts rounds of the game and keeps track of the overall tournament scores.
;;    It allows the user to decide whether to continue the tournament.
;; Parameters:
;;    - humanScore: The initial score of the human player at the beginning of the tournament.
;;    - computerScore: The initial score of the computer player at the beginning of the tournament.
;; Return Value: None (Void function)
;; Algorithm:
;;    1. Initialize the tournament scores with humanScore and computerScore.
;;    2. Determine the overall outcome of the tournament based on the scores.
;;    3. If the human player leads, start rounds with the human player as the first player and the computer player as the second player.
;;       - Play the rounds, update scores, and interact with the user for continuation.
;;    4. If the computer player leads, follow a similar process with the computer player starting.
;;    5. In the event of a tie, randomly select the first player and play the rounds.
;;    6. Display final tournament scores and end the tournament based on the user's decision.
;; Assistance Received: None
;; ********************************************************************* */
(defun tournament(humanScore computerScore)
 (cond
  ((> humanScore computerScore)
   (let* ((result (start-round 'Human 'Computer humanScore computerScore)))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (Enter 'y' to confirm!): ~%")
       (let ((response (read-line)))
         (cond
           ((string= (string-downcase response) "y")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")
            (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))

            ))))
   )
  )

  ((< humanScore computerScore)
   (let* ((result (start-round 'Computer 'Human humanScore computerScore)))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (Enter 'y' to confirm!): ~%")
       (let ((response (read-line)))
         (cond
           ((string= (string-downcase response) "y")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")
              (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))
            ))))
   )
  )

  (t
   (let* ((playerList (start-game))
          (result (start-round (first playerList) (second playerList) humanScore computerScore))
          (scores (calculate-score result)))
     (format t "-----Tournament Scores-----~%")
     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
     (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
     (format t "Continue the tournament? (Enter 'y' to confirm!): ~%")
     (let ((response (read-line)))
       (cond
         ((string= (string-downcase response) "y")
          (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
         (t
          (format t "Tournament ended.~%")
          (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))

          ))))
  )
  )
)

;; /* *********************************************************************
;; Function Name: declare-winner
;; Purpose: Declares the winner of a tournament or indicates a draw.
;; Parameters:
;;     - human-scores (integer): The total scores of the human player in the tournament.
;;     - computer-scores (integer): The total scores of the computer player in the tournament.
;; Return Value: None (output message to the console)
;; Algorithm: Compares the human and computer scores to determine the winner or if it's a draw and prints the corresponding message.
;; Assistance Received: None
;;********************************************************************* */
(defun declare-winner(humanScores computerScores)
  (cond(
      (> humanScores computerScores)
      (format t "You won the tournament!")

  )
  ((< humanScores computerScores)
      (format t "Computer won the tournament!"))
  (t
    (format t "It's a draw!")
  )
  )

)


;; /* *********************************************************************
;; Program Entry Point: Main Tournament Loop
;; Purpose: This block of code manages the flow of the program based on the user's response to the welcome message.
;; Parameters: None
;; Return Value: None (output messages and function calls)
;; Algorithm:
;;     - Display a welcome message and prompt for user response.
;;     - If the user's response is not equal to "1", load the game state.
;;     - Play a game using the loaded game state and calculate scores.
;;     - Display tournament scores and ask if the user wants to continue the tournament.
;;     - Based on the user's response, either continue the tournament or declare a winner.
;;     - If the user's response is "1," start a new tournament.
;; Assistance Received: None
;; ********************************************************************* */

(let* ((response (welcome)))
  (cond
    ((not(equal response "1"))
      (let ((game-state (load-game)))
          (let ((result (play-game (first game-state) (second game-state) (third game-state) (fourth game-state) (fifth game-state) (sixth game-state) (seventh game-state) (check-stones (first game-state)) (eighth game-state) (ninth game-state) )))
            (let* ((scores (calculate-score result)))
              (format t "-----Tournament Scores-----~%")
              (format t "Human Scores: ~a~%" (+ (first scores) (eighth game-state)))
              (format t "Computer Scores: ~a~%" (+ (second scores) (ninth game-state)))
              (format t "Continue the tournament? (Enter 'y' to confirm!): ~%")
              (let ((response (read-line)))
                (cond
                  ((string= (string-downcase response) "y")
                   (tournament (+ (first scores) (eighth game-state)) (+ (second scores) (ninth game-state))))
                  (t
                   (format t "Tournament ended.~%")
                   (declare-winner (+ (first scores) (eighth game-state)) (+ (second scores) (ninth game-state)))
                   ))))
          ))
    )
    (t
      (tournament 0 0))))

