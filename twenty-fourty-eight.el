;;; twenty-fourty-eight.el --- the game of twenty fourty eight using simple textual buffer
;;; Commentary:
;;; Nothing in particular
;;; Code:

(defconst 2048-size 4)
(defconst 2048-cell-size 15)
(defvar-local 2048-state [[0 0 0 0] [0 0 0 0]  [0 0 0 0] [0 0 0 0]])
(defvar-local 2048-game-over nil)
(defvar-local 2048-score 0)

;; (make-vector 4 (make-vector 4 0))

(defun 2048-cell (i j)
  "Return a cell of the game state.

  I: index of the row.
  J: index of the column."
  (elt (elt 2048-state i) j))

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

(defun 2048-merge-identicals-row (dir)
  "Merge identical cells at the end of each direction.

  DIR: one of the four directions."
  (let (k)
    (dotimes (r 2048-size)
      (dotimes (j (1- 2048-size))
        (setq k (if (eq dir :right) (- (1- 2048-size) j) j))
        (when (= (elt (elt 2048-state r) k) (elt (elt 2048-state r) (if (eq dir :right) (1- k) (1+ k))))
          (aset (elt 2048-state r) k (* (elt (elt 2048-state r) k) 2))
          (aset (elt 2048-state r) (if (eq dir :right) (1- k) (1+ k)) 0)
          (setq 2048-score (+ 2048-score (elt (elt 2048-state r) k))))))))

(defun 2048-merge-identicals-col (dir)
  "Merge identical cells at the end of each direction.

  DIR: one of the four directions."
  (let (k)
    (dotimes (c 2048-size)
      (dotimes (j (1- 2048-size))
        (setq k (if (eq dir :down) (- (1- 2048-size) j) j))
        (when (= (elt (elt 2048-state k) c) (elt (elt 2048-state (if (eq dir :down) (1- k) (1+ k))) c))
          (aset (elt 2048-state k) c (* (elt (elt 2048-state k) c) 2))
          (aset (elt 2048-state (if (eq dir :down) (1- k) (1+ k))) c 0)
          (setq 2048-score (elt (elt 2048-state k) c)))))))

(defun 2048-move-empty-cells (dir)
  "Move empty cells to the end of a matrix.

  DIR: one of the main four directions."
  (cond
   ((or (eq dir :left) (eq dir :right)) (2048-move-empty-cells-row dir))
   ((or (eq dir :up) (eq dir :down)) (2048-move-empty-cells-col dir))))

(defun 2048-merge-identicals (dir)
  "Merge identical cells along the given direction.

  DIR: one of the four directions."
  (cond
   ((or (eq dir :left) (eq dir :right)) (2048-merge-identicals-row dir))
   ((or (eq dir :up) (eq dir :down)) (2048-merge-identicals-col dir))))

(defun 2048-add-new-cell ()
  "Check to sea of there is any zero cells left."
  (let (positions pos i)
    (setq positions '())
    (dotimes (r 2048-size)
      (dotimes (c 2048-size)
        (when (= (2048-cell r c) 0)
          (setq positions (cons (cons r c) positions)))))
    (setq 2048-game-over (= (length positions) 0))
    (unless 2048-game-over
      (setq i (random (length positions)))
      (setq pos (elt positions i))
      (aset (elt 2048-state (car pos)) (cdr pos) (if (> (random) 0) 2 4)))
    2048-game-over))

(defun 2048-render ()
  "Create a text representation of the game matrix."
  (let (str-acc)
    (setq str-acc (cons (2048-top-line) str-acc))
    (dotimes (i 2048-size)
      (setq str-acc (cons (concat "|" (mapconcat '2048-val-to-string (elt 2048-state i) "|") "|") str-acc)
            str-acc (cons (2048-other-line) str-acc)))
    (mapconcat 'identity (reverse str-acc) "\n")))


(defun 2048-step (dir)
  "Move the numbers.

  DIR: one of the four directions"
  (2048-move-empty-cells dir)
  (2048-merge-identicals dir)
  (2048-add-new-cell)
  (2048-render))

(provide 'twenty-fourty-eight)
;;; twenty-fourty-eight.el ends here
