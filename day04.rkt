#!/usr/bin/racket
#lang racket

(define input (sort
    (file->lines "input04.txt")
    string<?))

;; each line is either
;; | [irrelevant] Guard #[id] begins shift
;; | [irrelevant] 00:[minute] falls asleep
;; | [irrelevant] 00:[minute] wakes up

;; A "soldier" is (cons [id] [list of minutes past midnight]).
;; First number is when they fall asleep; they wake up at next; etc.

(define (update-state line soldiers)
    ;; applies the "line" update to the current-id/soldiers state
    ;; and returns '(current-id soldiers)
    (let ((the-time (string->number (substring line 15 17))))
        (if
            (or
                (empty? soldiers)
                (char=? (string-ref line 19) #\G))
            ; "Guard #[id] begins shift"
            ; put new soldier into list
            (cons
                (list
                    (string->number
                        (second (regexp-match #rx"#([0-9]+)" line))))
                soldiers)
            ; "falls asleep" or "wakes up"
            ; update first soldier
            (cons
                (append (car soldiers) (list the-time))
                (cdr soldiers)))))

(define (time-asleep record)
        (if (empty? record) 0
            (+
                (- (second record) (first record))
                (time-asleep (cddr record)))))

(define (total-time-asleep soldier-id data)
    (foldl
        (lambda (soldier total) (+ total (time-asleep (cdr soldier))))
        0
        (filter (lambda (x) (= soldier-id (car x))) data)))

(define (asleep-during? record minute asleep)
    (cond
        ((empty? record) asleep) ; state doesn't change any more
        ((< minute (car record)) asleep)
        (else (asleep-during? (cdr record) minute (not asleep)))))
        
(define (count-asleep minute records)
    ; returns how many times the guard was asleep for this minute
    (count identity
        (map
            (lambda (record) (asleep-during? record minute #f))
            records)))

(define (count-asleep-by-id idminute data)
    (let ((id (first idminute))
          (minute (second idminute)))
        (count-asleep minute
            (map cdr
                (filter
                    (lambda (soldier) (= id (car soldier)))
                    data)))))

(let* ((soldiers-data (foldl update-state '() input))
       (soldier-ids (remove-duplicates (map car soldiers-data))))
    ; part 1
    (let* ((weakest-id (car
            (sort soldier-ids
                (lambda (x y)
                    (>
                        (total-time-asleep x soldiers-data)
                        (total-time-asleep y soldiers-data))))))
           (weakest-records (map cdr
                (filter
                    (lambda (soldier)
                        (= weakest-id (car soldier)))
                    soldiers-data))))
        ; find time at which weakest-id was most often asleep
        (println (* weakest-id (car
            (sort
                (range 0 60)
                (lambda (x y)
                    (>
                        (count-asleep x weakest-records)
                        (count-asleep y weakest-records))))))))
    ; part 2
    (println (apply * (car
        (sort
            (cartesian-product soldier-ids (range 0 60))
            (lambda (x y)
                (>
                    (count-asleep-by-id x soldiers-data)
                    (count-asleep-by-id y soldiers-data))))))))
