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