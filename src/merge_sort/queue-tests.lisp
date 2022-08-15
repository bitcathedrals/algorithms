; tests for make-queue. make sure it's setting up the tail pointer correctly.
(setq foo (make-queue))

(format t "~a" bar)

(setq bar (make-queue 1 2 3 4 5))

(car (cdr bar))

(car (car bar))

(queue-shift bar)

(queue-insert bar 1)

(queue-insert bar 2)

(queue-print (queue-join (make-queue) (make-queue 1 2 3 4)))

(queue-print (queue-join (make-queue 1 2 3 4) (make-queue 5 6 7 8)))

(setq baz (make-queue 1 2 3 4))

(queue-print baz)

(queue-append baz 5)
