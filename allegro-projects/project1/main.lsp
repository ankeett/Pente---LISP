(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")

(welcome)

(defun play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (print-2d-board board)
  (format t "~a's turn.~%" playerType)

  (cond
    (
      ;if someone wins
      ;(check-five board row col playerColor)
      ;(format t "~a wins!~%" playerType)
      ()
    )
    (
      ;if the board is full
      ()
      
    )
    
    (
      t 
      
      (cond
        (
          ;if computer's turn
          (equal playerType "Computer")
          (let* ((computer-move (computerMove))
                 (row (first computer-move))
                 (col (second computer-move))
                 (new-board (place-stone board row col playerColor)))
            (captures "Computer" playerCaptures "Human"  opponentCaptures)
            (format t "Computer move: ~a~%" (third computer-move))
              (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                ;;return the board and everything
                )
                ;;(calculate-score playerCaptures opponentCaptures "Computer"))
                (t
                    (cond
                        ((let* ((captured-board (check-capture new-board row col playerColor))
                              (next-playerCaptures (+ 1 playerCaptures)))
                          (cond
                            (captured-board
                            (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))
                            (t
                            (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures))))
                        ))

                    
                    ))
              ;;(play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures)
          ))
      (
        t 
        (equal playerType "Human")
        ;get user move
        (let* ((user-move (getUserMove))
              (row (first user-move))
              (col (second user-move)))
          (let* ((new-board (place-stone board row col playerColor)))
              (captures "Human" playerCaptures "Computer"  opponentCaptures)
            (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                ;;return the board and everything here
                (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 5))
                ;send 5 for 5 in a row to add to score
                ;;(calculate-score playerCaptures opponentCaptures "Human"))
                (t
                  (print "for capture")
                    (cond
                        ((let* ((captured-board (check-capture new-board row col playerColor))
                              (next-playerCaptures (+ 1 playerCaptures)
                              ))
                              (print captured-board)
                          (cond
                            (captured-board
                              (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))
                            (t
                              (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures))))
                        ))
                    ))
          )
        )
      )

    )

  )
))


(defun captures(playerType playerCapture opponentType opponentCapture)
  (princ playerType)
  (princ " Captures: ") 
  (princ playerCapture)
  (terpri)
  (princ opponentType)
  (princ " Captures: ")
  (princ opponentCapture)
  (terpri)
  (princ "--------------------------------")
  (terpri)

)

;; (defun calculate-score (playerCapture opponentCapture winnerType)
;;   (format t "Round Scores:~%")
;;   ;store the score and send the winner and looser
;;   (cond
;;     ( (equal winnerType "Human")
;;         (princ "Human Scores: ")
;;         (princ (+ 5 playerCapture))
;;         (terpri)
;;         (princ "Computer Scores: ")
;;         (princ opponentCapture)
;;         (terpri)
;;     )
;;     (t
;;       (princ "Human Scores: ")
;;         (princ  opponentCapture)
;;         (terpri)
;;         (princ "Computer Scores: ")
;;         (princ (+ 5 playerCapture))
;;         (terpri)
;;     )
;;   )
;;   ;(ask-for-next-game winnerType)
;; )

;;(list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
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
      ((equal winnerType "Human")
        (format t "Human Scores: ~a~%" (+ winnerCapture isFiveinarow))
        (format t "Computer Scores: ~a~%" opponentCapture)
        (list (+ winnerCapture isFiveinarow) opponentCapture))
      (t
        (format t "Human Scores: ~a~%" opponentCapture)
        (format t "Computer Scores: ~a~%" (+ winnerCapture isFiveinarow))
        (list opponentCapture (+ winnerCapture isFiveinarow))))
    ))


(defun ask-for-next-game(winner loser)
  (format t "Do you want to play one more round?(y/n) ~%")
  (finish-output)
  (let* ((user-input (read-line)))
    (format t "You entered: ~a~%" user-input)
    (cond 
      ((equal user-input "y")
       (start-game))
      ((equal user-input "n")
       (quit-game))
      (t (format t "Invalid input.~%"))))
)


(defun quit-game()
  (format t "Do you want to Serialize (y/n)")

)

;; (defun start-round(firstPlayerType secondPlayerType )
;;   (let* ((my-2d-board (make-2d-board 19 19)))
;;     (play-game my-2d-board "W" firstPlayerType "B" secondPlayerType 0 0)))

(defun start-round (firstPlayerType secondPlayerType)
  (let* ((my-2d-board (make-2d-board 19 19))
         (game-result (play-game my-2d-board "W" firstPlayerType "B" secondPlayerType 0 0)))
    (values game-result)))


;; (let* ((playerList (get-welcome-input)))
;;   (start-round (first playerList) (second playerList))
;; )



(defun tournament(humanScore computerScore)

  ;;checks human and computer score
  ;;if equal toss the coin and determine first and second playerType


  ;;start-round(firstPlayerType, secondPlayerType)
      ;;play-round()
      ;;play-round should return everything  like board winnerColor winnerType opponentColor opponentType winnerCapture opponentCapture
      ;;this again returned by start-round
  
  ;;calls calculateScore
    ;;takes board for 4 in a row
    ;;capture winnerType for 5 in a row
    ;;winnerCapture and opponentCapture with winnerType and opponentType to calculate captureScore

  ;;calculate score returns humanScore and computerScore
  ;;recursion tournament again


  ;; (cond
  ;;   ((> humanScore computerScore)
  ;;   (let* ((result (start-round "Human" "Computer")))
  ;;     (let* ((scores (calculate-score result)))
  ;;           (format t "-----Tournament Scores-----~%")
  ;;       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore))))
  ;;   )
    
  ;;   ((< humanScore computerScore)
  ;;   (let* ((result (start-round "Computer" "Human")))
  ;;     (let* ((scores (calculate-score result)))
  ;;           (format t "-----Tournament Scores-----~%")
  ;;       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore)))
  ;;   ))
    
  ;;   (t
  ;;   (let* ((playerList (start-game))
  ;;           (result (start-round (first playerList) (second playerList)))
  ;;           (scores (calculate-score result)))
  ;;       (format t "-----Tournament Scores-----~%")
  ;;     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore)))
  ;;     )
  ;; )


 (cond
  ((> humanScore computerScore)
   (let* ((result (start-round "Human" "Computer")))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (yes/no): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "yes")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")))))
   )
  )

  ((< humanScore computerScore)
   (let* ((result (start-round "Computer" "Human")))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (yes/no): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "yes")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")))))
   )
  )

  (t
   (let* ((playerList (start-game))
          (result (start-round (first playerList) (second playerList)))
          (scores (calculate-score result)))
     (format t "-----Tournament Scores-----~%")
     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
     (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
     (format t "Continue the tournament? (yes/no): ~%")
     (let ((response (read-line)))
       (cond
         ((string= response "yes")
          (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
         (t
          (format t "Tournament ended.~%")))))
  )
)


)


(tournament 0 0)