(defun computerMove()
  (let* ((row (random 19))
         (col (random 19))
         (adjusted-row (- 19 row)) ;suspected error
         (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row)))
         )
    (list row col move)))
