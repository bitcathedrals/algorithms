;----------------------------------------------------------------------
; merge-sort.lisp: written by Mike Mattie
;----------------------------------------------------------------------

; I noticed while looking at algorithms that this sorting technique is
; actually well suited to LISP which is rare. It consists almost entirely
; of consuming lists.

; merge-ascending takes two lists: A and B. It repeatedly consumes the
; lesser of the two values from the lists assuming that lists A and B
; are ordered.
;
; merge-sort uses a divide and conquer strategy to split the range into two
; parts and recurse into each half, merging the resulting lists.

; most of the lisp functions that do things like counting the length and
; appending elements or other lists takes O(n). Queue is a implementation
; that uses a cons cell to keep track of the head and the tail. This makes
; it possible to efficiently append to the end, and take items from the
; beginning.
;
; It is only O(n) on queue creation, all other operations are O(1)
(load "queue.lisp")

;
; sorting implementation
;

(defun merge-ascending (result queue-a queue-b)
  ; (format t "result: ~a queue-a: ~a queue-b: ~a ~%" result queue-a queue-b)

  (if (or (queue-is-empty queue-a) (queue-is-empty queue-b))
    (cond
      ((queue-is-not-empty queue-a) (queue-join result queue-a))
      ((queue-is-not-empty queue-b) (queue-join result queue-b))
      (t result))
    (if (< (queue-peek-head queue-a) (queue-peek-head queue-b))
      (merge-ascending (queue-append result (queue-peek-head queue-a)) (queue-shift queue-a) queue-b)
      (merge-ascending (queue-append result (queue-peek-head queue-b)) queue-a (queue-shift queue-b))) ))

(defun middle (i n)
  (+ (values (floor (/ (- n i) 2))) i) )

(defun merge-sort (data i n)
  ; (format t "in merge-sort with ~a ~a ~%" i n)
  (cond
    ((equal (- n i) 0) (make-queue))
    ((equal (- n i) 1) (make-queue (aref data i)))
    ((equal (- n i) 2) (if (< (aref data i) (aref data (+ i 1)))
                         (make-queue (aref data i) (aref data (+ i 1)))
                         (make-queue (aref data (+ i 1)) (aref data i))))
    (t (merge-ascending (make-queue)
         (merge-sort data i (middle i n))
         (merge-sort data (middle i n) n))) ))

;
; auxilary functions for loading and timing execution
;

; This is fragile, doesn't handle blank lines well. could use a bit of work.
(defun numbers-from-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defun list-to-vector (input)
  (let
    ((table (make-array (length input) :fill-pointer 0)) )

    (mapcar (lambda (x) (vector-push x table)) input)
    table))

(defun execute-sort (numbers-list)
  (let*
    ((data (list-to-vector numbers-list))
     (n (length numbers-list))
     (sorted (time (merge-sort data 0 n))))

    (if (< n 25)
      (format t "sorted = ~a~%" sorted)
      (format t "sorted = to long to print~%"))

    sorted))

; entry point for execution.
(defun main ()
  (format t "merge-sort.lisp -> running now~%" )

  ; the first argument is a file of newline seperated integers.
  (let
    ((filename (cadr sb-ext:*posix-argv*)))

    (execute-sort (numbers-from-file filename)) ))

(defun sort-file (filename)
  (execute-sort (numbers-from-file filename)))


