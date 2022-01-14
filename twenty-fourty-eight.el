;;; twenty-fourty-eight.el --- the game of twenty fourty eight using simple textual buffer
;;; Commentary:
;;; Nothing in particular
;;; Code:

(defconst 2048-size 4)
(defconst 2048-cell-size 15)
(defvar-local 2048-state [[0 0 0 0] [0 0 0 0]  [0 0 0 0] [0 0 0 0]])
(defvar-local 2048-game-over nil)

;; (make-vector 4 (make-vector 4 0))

(defun 2048-top-line ()
  "Create string for top line of the table."
  (make-string (* 2048-size 2048-cell-size) ?_))

(defun 2048-other-line ()
  "Create string for every other horizontal line except for the top line."
  (let ((str-acc '()))
    (dotimes (i 2048-size)
      (setq str-acc (cons (concat (make-string (- 2048-cell-size (if (= i 0) 2 1)) ?_) (string ?|)) str-acc)))
    (setq str-acc (cons "|" str-acc))
    (eval (cons 'concat str-acc))))

(defun 2048-val-to-string (val)
  "Convert a value into a string by adding space margin to left and right.

  VAL: integer in a cel of the state"
  (let (str-val margin)
    (setq str-val (number-to-string val)
          margin (/ (- 2048-cell-size (length str-val)) 2))
    (concat (make-string margin ?\s) str-val (make-string (- margin 1) ?\s))))

(defun 2048-remove-empty-cells-row (dir &optional state)
  "Remove all the empty cells in the matrix along the direction of all rows.

   DIR: one of the four directions.
   STATE: the game state matrix."
  (let (i matrix)
    (setq matrix (if state state 2048-state))
    (dotimes (r 2048-size)
      (setq i (if (eq dir :right) (1- 2048-size) 0))
      (dotimes (j 2048-size)
        (setq j (if (eq dir :right) (- (1- 2048-size) j) j))
        (unless (= (elt (elt matrix r) j) 0)
          (aset (elt matrix r) i (elt (elt matrix r) j))
          (aset (elt matrix r) j 0)
          (setq i (if (eq dir :right) (1- i) (1+ i))))))))

(defun 2048-move (dir)
  "Move the numbers.

  DIR: one of the four directions"
  (let ()
    (cond
     ((eq dir :left)
      (dotimes (i 2048-size)
        (dotimes (j 2048-size)
          ))))
    ))


(defun 2048-create-text-matrix ()
  "Create a text representation of the game matrix."
  (let (str-acc)
    (setq str-acc (cons (2048-top-line) str-acc))
    (dotimes (i 2048-size)
      (setq str-acc (cons (concat "|" (mapconcat '2048-val-to-string (elt 2048-state i) "|") "|") str-acc)
            str-acc (cons (2048-other-line) str-acc)))
    (mapconcat 'identity (reverse str-acc) "\n")))

(provide 'twenty-fourty-eight)
;;; twenty-fourty-eight.el ends here
