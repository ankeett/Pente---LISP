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
                (calculate-score playerCaptures opponentCaptures "Computer"))
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
                (calculate-score playerCaptures opponentCaptures "Computer"))
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

(defun calculate-score (playerCapture opponentCapture winnerType)
  (format t "Round Scores:~%")
  ;store the score and send the winner and looser
  (cond
    ( (equal winnerType "Human")
        (princ "Human Scores: ")
        (princ (+ 5 playerCapture))
        (terpri)
        (princ "Computer Scores: ")
        (princ opponentCapture)
        (terpri)
    )
    (t
      (princ "Human Scores: ")
        (princ  opponentCapture)
        (terpri)
        (princ "Computer Scores: ")
        (princ (+ 5 playerCapture))
        (terpri)
    )
  )
  ;(ask-for-next-game winnerType)
)

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


(defun start-round(firstPlayerType secondPlayerType )
  (let* ((my-2d-board (make-2d-board 19 19)))
    (play-game my-2d-board "W" firstPlayerType "B" secondPlayerType 0 0)))

(let* ((playerList (get-welcome-input)))
  (start-round (first playerList) (second playerList))
)

