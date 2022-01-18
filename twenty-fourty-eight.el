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

(defun 2048-move-empty-cells-row (dir)
  "Remove all the empty cells in the matrix along the direction of all rows.

   DIR: one of the four directions."
  (let (i k)
    (dotimes (r 2048-size)
      (setq i (if (eq dir :right) (1- 2048-size) 0))
      (dotimes (j 2048-size)
        (setq k (if (eq dir :right) (- (1- 2048-size) j) j))
        (unless (= (elt (elt 2048-state r) k) 0)
          (unless (= i k)
            (aset (elt 2048-state r) i (elt (elt 2048-state r) k))
            (aset (elt 2048-state r) k 0)
          (setq i (if (eq dir :right) (1- i) (1+ i)))))))))


(defun 2048-move-empty-cells-col (dir)
  "Remove all empty cells in the matrix alogn the direction in all columns.

   DIR: either :up or :down."
  (let (i r)
    (dotimes (c 2048-size)
      (setq i (if (eq dir :down) (1- 2048-size) 0))
      (dotimes (k 2048-size)
        (setq r (if (eq dir :down) (- (1- 2048-size) k) k))
        (unless (= (elt (elt 2048-state r) c) 0)
          (unless (= i r)
            (aset (elt 2048-state i) c (elt (elt 2048-state r) c))
            (aset (elt 2048-state r) c 0))
          (setq i (if (eq dir :down) (1- i) (1+ i))))))))

(defun 2048-merge-identicals (dir)
  "Merge identical cells at the end of each direction.

  DIR: one of the four directions.")

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
