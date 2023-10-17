(defun welcome()
    (format t "Welcome to Pente!~%")
    (format t "You will be playing against the computer.~%")
    (format t "Choose:~%")
    (format t "1. Start a new game~%")
    (format t "2. Load a saved game~%")
    (get-welcome-input)
)


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



(defun toss-coin()
    (+ 1 (random 2))
)

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
