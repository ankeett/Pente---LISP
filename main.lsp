(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")
(load "test.lsp")


(defun play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount)
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
          (string= playerType 'Computer)
          (format t "Do you want to quit? (Enter 'y' to quit)")
          (finish-output)
          (let* ((userInput (read-line)))
            (format t "You entered: ~a~%" userInput)
              (
                cond 
                (
                  (string= userInput "y")
                  (quit-the-game board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
                  
                )
              )
          )

          (let* ((computer-move (computerMove board playerColor moveCount))
                 (row (first computer-move))
                 (col (second computer-move))
                 (new-board (place-stone board row col playerColor)))
            (format t "Computer move: ~a~%" (third computer-move))
            ;;(captures 'Computer playerCaptures 'Human  opponentCaptures)
              (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                (print-2d-board new-board)
                ;;return the board and everything
                (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 5)

                )
                ;;(calculate-score playerCaptures opponentCaptures "Computer"))
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
                              (print-2d-board captured-board)
                              (list captured-board playerColor playerType opponentColor opponentType next-playerCaptures opponentCaptures 0)) 
                              
                            
                            (t
                            
                            (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount)))))
                          (t
                            (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount)))))
                      ))

                    
                    ))
          ))
      (
        t 
        (string= playerType 'Human)
        ;get user move
        (let* ((user-move (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures moveCount))
              (row (first user-move))
              (col (second user-move)))
          (let* ((new-board (place-stone board row col playerColor)))
              ;; (captures 'Human playerCaptures 'Computer  opponentCaptures)
            (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                (print-2d-board new-board)
                ;;return the board and everything here
                (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 5))
                ;send 5 for 5 in a row to add to score
                ;;(calculate-score playerCaptures opponentCaptures "Human"))
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
                              (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount)))))
                          (t
                            (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures (+ 1 moveCount)))))
                      ))

                    ))
          )
        )
      )

    )

  )
))


(defun captures(playerType playerCapture opponentType opponentCapture)
  (format t "--------------------------------~%")
  (format t "~a Captures: ~a~%" playerType playerCapture)
  (format t "~a Captures: ~a~%" opponentType opponentCapture)
  (format t "--------------------------------~%"))


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
      ((equal winnerType 'Human)
        (format t "Human Scores: ~a~%" (+ winnerCapture isFiveinarow (- (count-four board winnerColor) 1)))
        (format t "Computer Scores: ~a~%" (+ opponentCapture (count-four board opponentColor)))
        (list (+ winnerCapture isFiveinarow (- (count-four board winnerColor) 1 )) (+ opponentCapture (count-four board opponentColor))))
      (t
        (format t "Human Scores: ~a~%" (+ opponentCapture (count-four board opponentColor)))
        (format t "Computer Scores: ~a~%" (+ winnerCapture isFiveinarow (- (count-four board winnerColor) 1)))
        (list (+ opponentCapture (count-four board opponentColor)) (+ winnerCapture isFiveinarow (- (count-four board winnerColor) 1)))))
    ))

(defun start-round (firstPlayerType secondPlayerType)
  (let* ((my-2d-board (make-2d-board 19 19))
         (game-result (play-game my-2d-board 'W firstPlayerType 'B secondPlayerType 0 0 1)))
    (values game-result)))


(defun tournament(humanScore computerScore)

 (cond
  ((> humanScore computerScore)
   (let* ((result (start-round 'Human 'Computer)))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? ('y' to confirm): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "y")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")
            (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))

            ))))
   )
  )

  ((< humanScore computerScore)
   (let* ((result (start-round 'Computer 'Human)))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? ('y' to confirm.): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "y")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")
              (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))
            ))))
   )
  )

  (t
   (let* ((playerList (start-game))
          (result (start-round (first playerList) (second playerList)))
          (scores (calculate-score result)))
     (format t "-----Tournament Scores-----~%")
     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
     (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
     (format t "Continue the tournament? ('y' to confirm.): ~%")
     (let ((response (read-line)))
       (cond
         ((string= response "y")
          (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
         (t
          (format t "Tournament ended.~%")
          (declare-winner (+ (first scores) humanScore) (+ (second scores) computerScore))

          ))))
  )
  )
)

;;(tournament 0 0)

;;load game state
;;call play-game
;; call round score and call tournament with the updated score
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


(let* ((response (welcome)))
  (cond
    ((not(equal response "1"))
      (let ((game-state (load-game)))
          (format t "Loaded game state: ~A~%" game-state)
          (let ((result (play-game (first game-state) (second game-state) (third game-state) (fourth game-state) (fifth game-state) (sixth game-state) (seventh game-state) (check-stones (first game-state)))))
            (let* ((scores (calculate-score result)))
              (format t "-----Tournament Scores-----~%")
              (format t "Human Scores: ~a~%" (+ (first scores) (eighth game-state)))
              (format t "Computer Scores: ~a~%" (+ (second scores) (ninth game-state)))
              (format t "Continue the tournament? ('y' to confirm): ~%")
              (let ((response (read-line)))
                (cond
                  ((string= response "y")
                   (tournament (+ (first scores) (eighth game-state)) (+ (second scores) (ninth game-state))))
                  (t
                   (format t "Tournament ended.~%")
                   (declare-winner (+ (first scores) (eighth game-state)) (+ (second scores) (ninth game-state)))
                   ))))
          ))
    )
    (t
      (tournament 0 0))))

