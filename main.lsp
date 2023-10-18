;; ************************************************************
;; * Name:  Ankit Kafle                                       *
;; * Project:  Pente, Lisp                                    *
;; * Class: CMPS-366|01, Organization of Programming Languages*
;; * Date:  Oct 18, 2023                                      *
;; ************************************************************



(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")
(load "Tournament.lsp")
(load "FileRead.lsp")

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