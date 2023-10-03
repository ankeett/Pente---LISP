(defun welcome()
    (format t "Welcome to Pente!~%")
    (format t "You will be playing against the computer.~%")
    (format t "Choose:~%")
    (format t "1. Start a new game~%")
    (format t "2. Load a saved game~%"))

(defun get-welcome-input ()
  (format t "Enter something: ")
  (finish-output)
  (let ((user-input (read-line)))
    (format t "You entered: ~a~%" user-input)

    (cond ((equal user-input "1")
           (let ((result (start-game)))
             result))
          ((equal user-input "2") (load-game))
          (t (format t "Invalid input.~%")
             (get-welcome-input)))    
  )
)


(defun toss-coin()
    (print (+ 1 (random 2)))
)

(defun start-game()
    (format t "Starting a new game~%")
    (format t "Tossing a coin to decide who plays first~%")
    (princ "Enter 1 for heads or 2 for tails: ")
    (terpri)
    (finish-output)
    (let* ((tossed (toss-coin)))
        (let* ((user-input (read-line)))
            (format t "You entered: ~a~%" user-input)

            (cond ((equal user-input tossed)
                (princ "You won the toss.")
                (terpri)
                (princ "You will play: White")
                (terpri)
                (list "Human" "Computer"))
                
                (t
                    (princ "Computer won the toss.")
                    (terpri)
                    (princ "You will play: Black")
                    (terpri)
                    (list "Computer" "Human")
                )    
            )
    
        )
    )
)

(defun load-game()
    (print "Loading a saved game")
)




