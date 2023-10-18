;; /* *********************************************************************
;; Function Name: welcome
;; Purpose: Display a welcome message to the user and prompt for the choice to start a new game or load a saved game.
;; Parameters: None
;; Return Value: None (output messages and function calls)
;; Algorithm:
;;     - Display a welcome message for the Pente game.
;;     - Inform the user that they will be playing against the computer.
;;     - Provide the user with two choices: start a new game (1) or load a saved game (2).
;;     - Call the `get-welcome-input` function to get the user's choice.
;; Assistance Received: None
;; ********************************************************************* */

(defun welcome()
    (format t "Welcome to Pente!~%")
    (format t "You will be playing against the computer.~%")
    (format t "Choose:~%")
    (format t "1. Start a new game~%")
    (format t "2. Load a saved game~%")
    (get-welcome-input)
)

;; /* *********************************************************************
;; Function Name: get-welcome-input
;; Purpose: Prompt the user for their choice and return it.
;; Parameters: None
;; Return Value: "1" if the user chooses to start a new game, "2" if they choose to load a saved game.
;; Algorithm:
;;     - Prompt the user to enter "1" to start a new game or "2" to load a game.
;;     - Read the user's input.
;;     - If the input is "1" or "2," return the corresponding choice.
;;     - If the input is invalid, display an error message and prompt the user again.
;; Assistance Received: None
;; ********************************************************************* */
(defun get-welcome-input ()
  (format t "Enter 1 to start a new game, 2 to load a game: ~%")
  (let ((user-input (read-line)))
    (cond
      ((equal user-input "1")
       "1")
      ((equal user-input "2")
       "2")
      (t
       (format t "Invalid input.~%")
       (get-welcome-input)))))


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
;; Function Name: deserialize
;; Purpose: Deserialize a saved game state from a file and extract the game data.
;; Parameters: None
;; Return Value:
;;     - A list containing the deserialized game state with the following elements:
;;         - (first) the game board.
;;         - (second) the human player's captures.
;;         - (third) the computer player's captures.
;;         - (fourth) the human player's score.
;;         - (fifth) the computer player's score.
;;         - (sixth) the type of the next player (Human or Computer).
;;         - (seventh) the color of the next player (Black or White).
;; Algorithm:
;;     1. Prompt the user to enter the filename of the saved game for deserialization.
;;     2. Attempt to open the specified file for reading.
;;     3. If the file doesn't exist, handle the file-error condition and prompt the user to enter a valid filename.
;;     4. Read the deserialized data from the file.
;;     5. Extract the necessary information from the deserialized data, such as the board, captures, scores, next player's type, and color.
;;     6. Determine the type of the next player based on the deserialized data.
;;     7. Return a list containing the extracted game state elements.
;; Assistance Received: None
;; ********************************************************************* */
(defun deserialize()
  (format t "Enter filename to deserialize: ~%")
  (finish-output)

  (let ((filename (read-line)))
    (handler-case
        (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
          (let ((data (read stream)))
            ;; Process the deserialized data as needed
            ;; For example, you can access elements of the data list
            (let* ((board (first data))
                    (humanCapture (second data))
                    (humanScore (third data))
                    (computerCapture (fourth data))
                    (computerScore (fifth data))
                    (nextPlayerType (sixth data))
                    (color-string (seventh data)))
              ; Continue processing the data
              (cond 
                ((string= nextPlayerType 'Human)
                 (list board humanCapture computerCapture humanScore  computerScore nextPlayerType color-string))
                (t
                 (list board computerCapture humanCapture humanScore computerScore nextPlayerType color-string))))
            ))
      (file-error (e)
        (format t "File ~A does not exist.~%" filename)
        (finish-output)
        (deserialize))))
  )
