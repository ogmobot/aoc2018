#!/usr/bin/sbcl --script
; indices 5 and 36 are the important ones

(defun update-graph (a b graph)
    ;; given two chars, (A required for B), update the given graph
    (acons b (cons a (cdr (assoc b graph))) graph))

(defun get-graph-and-letters (filename)
    ;; graph is of the form
    ;; ((char . requirement-list) ... )
    (with-open-file (input-file filename)
        (let ((lines (loop for line = (read-line input-file nil)
                while line collect line)))
            (cons
                (reduce
                    (lambda (graph line)
                        (update-graph (aref line 5) (aref line 36) graph))
                    lines
                    :initial-value nil)
                (sort
                    (remove-duplicates
                        (append
                            (mapcar (lambda (line) (aref line  5)) lines)
                            (mapcar (lambda (line) (aref line 36)) lines)))
                    #'char<)))))

(defun next-options (current-path graph letters)
    (remove-if
        (lambda (letter)
            (or
                ; don't go place we've already been
                (member letter current-path)
                ; are any requirements for this letter missing?
                (let ((reqs (cdr (assoc letter graph))))
                    (remove-if (lambda (r) (member r current-path)) reqs))))
        letters))

(defun update-work (todo done graph workers worker-list time-function elapsed)
    ;(format t "~s ~a~%" (mapcar #'car worker-list) elapsed)
    (cond
        ;; If there is nothing to do, and all workers are finished, we're done
        ((and (null todo) (null worker-list)) elapsed)
        ;; If there are free workers, start working
        ((and todo (< (length worker-list) workers) (next-options done graph todo))
            (let ((next-item (car (next-options done graph todo))))
                (update-work
                    (remove next-item todo)
                    done
                    graph
                    workers
                    (cons
                        (cons next-item (funcall time-function next-item))
                        worker-list)
                    time-function
                    elapsed)))
        ;; Otherwise, all workers tick down (and finished workers become free)
        (t
            (update-work
                todo
                (append
                    (mapcar #'car (remove-if-not (lambda (p) (= 1 (cdr p))) worker-list))
                    done)
                graph
                workers
                (mapcar (lambda (p) (cons (car p) (- (cdr p) 1)))
                    (remove-if (lambda (p) (= 1 (cdr p))) worker-list))
                time-function
                (+ 1 elapsed)))))

(defun do-the-work (seq graph workers time-function)
    (update-work seq nil graph workers nil time-function 0))
    
(let* ((tmp (get-graph-and-letters "input07.txt"))
       (graph (car tmp))
       (letters (cdr tmp))
       (path nil))
    ;part 1
    (loop while (next-options path graph letters)
        ; letters is already sorted alphabetically
        do (setf path
                (cons
                    (car (next-options path graph letters))
                    path)))
    (format t "~a~%" (coerce (reverse path) 'string))
    ; part 2
    ; Assume we should greedily take the first action available from
    ; part 1's solution
    (format t "~a~%"
        (do-the-work
            (reverse path)
            graph
            5
            (lambda (c) (+ 61 (position c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))))
