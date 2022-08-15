(defun make-queue (&rest initial-values)
  (let
    ((queue (cons initial-values nil))
     (tail initial-values))

    ; (format t "~a initial values" initial-values)

    (loop
      while (cdr tail)
      do (setq tail (cdr tail)) )

    (rplacd queue tail)

    ; the while loop stops at the end of the list. terminate the list
    ; properly.
    (if (queue-tail queue)
      (rplacd (queue-tail queue) nil))

    queue))

(defun queue-is-empty (queue)
  (not (queue-head queue)) )

(defun queue-is-not-empty (queue)
  (if (queue-head queue)
    t
    nil))

(defun queue-clear (queue)
  (rplaca queue nil)
  (rplacd queue nil))

(defun queue-print (queue)
  (format t "head is: ~a, tail is: ~a ~%"
    (queue-head queue) (queue-tail queue)) )

(defun queue-head (queue)
  (car queue))

(defun queue-tail (queue)
  (cdr queue))

(defun queue-insert (queue value)
  (let
    ((new-head (cons value (queue-head queue)) ))

    ; sync the cdr if cdr is nil
    (if (not (queue-tail queue))
      (rplacd queue new-head))

    (rplaca queue new-head)

    new-head))

(defun queue-next (queue)
  (if (or (not queue) (not (queue-head queue)))
    nil
    (let
      ((value (car (queue-head queue))))

      (rplaca queue (cdr (queue-head queue)))

      (if (not (queue-head queue))
        (rplacd queue nil))

      value) ))

(defun queue-shift (queue)
  (queue-next queue)
  queue)

(defun queue-append (queue value)
  (let
    ((new-tail (cons value nil)))

    (if (queue-is-empty queue)
      (rplaca queue new-tail)
      (if (queue-tail queue)
        (rplacd (queue-tail queue) new-tail)
        (rplacd (queue-head queue) new-tail) ))

    (rplacd queue new-tail)

    queue))

(defun queue-join (queue-a queue-b)
  (block fn
    (if (or (not queue-a) (not queue-b))
      (return-from fn nil))

    (if (not (queue-head queue-a))
      (return-from fn (if (queue-head queue-b) queue-b nil)))

    (if (not (queue-head queue-b))
      (return-from fn queue-a))

    (rplacd (queue-tail queue-a) (queue-head queue-b))
    (rplacd queue-a (queue-tail queue-b))

    queue-a))

(defun queue-list (queue)
  (queue-head queue))

(defun queue-peek-head (queue)
  (car (queue-head queue)))

(defun queue-peek-tail (queue)
  (car (queue-tail queue)))





