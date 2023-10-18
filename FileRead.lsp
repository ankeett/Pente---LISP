;; /* *********************************************************************
;; Function Name: serialize
;; Purpose: Serializes the game state and saves it to a file.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - playerColor (any): The symbol representing the player's color.
;; - playerType (any): The type of the player (e.g., human or computer).
;; - opponentColor (any): The symbol representing the opponent's color.
;; - opponentType (any): The type of the opponent (e.g., human or computer).
;; - playerCaptures (integer): The number of captures made by the player.
;; - opponentCaptures (integer): The number of captures made by the opponent.
;; - humanTournament (integer): A number representing the human player's tournament score.
;; - computerTournament (list): A number representing the computer player's tournament score.
;; Return Value: None (void function), but it serializes the game state to a file and exits the game.
;; Algorithm:
;; 1. Prompts the user to enter a filename for saving the game state.
;; 2. Reads the filename provided by the user.
;; 3. Determines the color of the player ('Black' or 'White') based on the playerColor parameter.
;; 4. Constructs a data structure containing game state information, including the board, playerCaptures, humanTournament, opponentCaptures, computerTournament, playerType, and color.
;; 5. Opens the file with the provided filename and writes the data structure to the file.
;; 6. Informs the user that the file was saved successfully and exits the game.
;; Assistance Received: None
;; ********************************************************************* */
(defun serialize(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures humanTournament computerTournament)
  (format t "Enter filename: ~%")
  (format t "~A~%" playerType)
  (finish-output)

  (let* ((filename (read-line))
         (color-string
          (cond
            ((equal playerColor 'B) 'Black)
            (t 'White)))
         (data
          (cond
            ((string= playerType 'Human)
            (format t "test~%")
             (list board playerCaptures humanTournament opponentCaptures computerTournament playerType color-string ))
            (t
             (list board opponentCaptures humanTournament playerCaptures computerTournament playerType color-string ))))
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