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
            (scores "Computer" playerCaptures "Human"  opponentCaptures)
            (format t "Computer move: ~a~%" (third computer-move))
              (cond ((check-five new-board row col playerColor)
                  (format t "Five in a row.~%" ))
                (t 
                  (format t "No five in a row.~%")
                  (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures)))
          )
      )
      (
        t 
        (equal playerType "Human")
        ;get user move
        (let* ((user-move (getUserMove))
              (row (first user-move))
              (col (second user-move)))
          (let* ((new-board (place-stone board row col playerColor)))
              (scores "Human" playerCaptures "Computer"  opponentCaptures)
            (cond ((check-five new-board row col playerColor)
                (format t "Five in a row.~%"))
              (t 
                (format t "No five in a row.~%")
                (cond
                  (
                    ;check for capture
                    ;give either board or t/nil
                    ;increase the score if tru



                  )
                  (
                    t
                  )
                )
                (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures)))
          )
        )
      )

    )

  )
)
)

(defun scores(playerType playerCapture opponentType opponentCapture)
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


(defun check-game-over (board)
  ; Implement game-over condition logic here
  nil)

(defun start-round(firstPlayerType secondPlayerType )
  (let* ((my-2d-board (make-2d-board 19 19)))
    (play-game my-2d-board "W" "Computer" "B" "Human" 0 0)))

(let* ((playerList (get-welcome-input)))
  (start-round (first playerList) (second playerList))
)



