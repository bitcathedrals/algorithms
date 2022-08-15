(merge-ascending (make-queue) (make-queue 0 4 8 10 12) (make-queue 2 5 9 13))


; merge ascending tests
(queue-print (merge-ascending foo (make-queue 0 4 8 10 12) (make-queue 2 5 9 13)))

(setq foo (make-queue))

(queue-append foo 5)

(queue-print foo)

foo



(merge-ascending nil '(2 4 9 10 15 22) '(5 13))

; debugging tool
(defun print-findings (name list)
  (progn
    (format t "sorted for [~a] -> ~a ~%" name list)
    list))

; test merges

(merge-sort (vector) 0 0)

(merge-sort (vector 10) 0 1)

(merge-sort (vector 10 5) 0 2)

(merge-sort (vector 10 5 6) 0 3)

(merge-sort (vector 10 5 6 2) 0 4)

(merge-sort (vector 10 5 6 2 12) 0 5)

(merge-sort (vector 10 5 6 2 12 74) 0 6)

(merge-sort (vector 7 2 5 10 11 14 6 12 21) 0 9)

(merge-sort (vector 14 13 12 11 10 9 8 7 6 5 4 3 2 1) 0 14)


(merge-sort (vector 1 6 4 3 5 8 7 12 15 21) 0 9)

(merge-sort (vector 2 3 1) 0 3)


(numbers-from-file "numbers.txt")

(list-to-vector (numbers-from-file "numbers.txt"))

(let*
  ((numbers (list-to-vector (numbers-from-file "numbers.txt")))
   (size (length numbers))
   (sorted (merge-sort numbers 0 size)) )

  (format t "~a" sorted))

(sort-file "numbers.txt")


; knuth shuffle
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(setq foo (vector 5 6 3 2 7 9 8))

(nshuffle foo)


(nshuffle '(1 2 3 4 5 6 7 8 9 10))
