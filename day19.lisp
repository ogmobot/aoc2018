;; run this with "sbcl --load <this>"
(require :cl-ppcre)
(declaim (optimize (speed 3) (safety 0)))

;;; input handling functions ;;;

(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun parse-line (text)
    (let* ((actual (car (cl-ppcre:all-matches-as-strings "[^;]*" text)))
           (words (cl-ppcre:all-matches-as-strings "[^\\s]+" actual)))
        (cons
            (car words)
            (mapcar #'parse-integer (cdr words)))))

;;; opcodes ;;;

(defvar opcodes
    ;; each opcode is a function that takes four arguments (a b c regs)
    ;; where 'regs is a vector of 4 items.
    ;; Mutates the vector and returns it.
    (list
        (cons "addr"
            (lambda (a b c regs)
                (setf (aref regs c) (+ (aref regs a) (aref regs b)))
                regs))
        (cons "addi"
            (lambda (a b c regs)
                (setf (aref regs c) (+ (aref regs a) b))
                regs))
        (cons "mulr"
            (lambda (a b c regs)
                (setf (aref regs c) (* (aref regs a) (aref regs b)))
                regs))
        (cons "muli"
            (lambda (a b c regs)
                (setf (aref regs c) (* (aref regs a) b))
                regs))
        (cons "banr"
            (lambda (a b c regs)
                (setf (aref regs c) (logand (aref regs a) (aref regs b)))
                regs))
        (cons "bani"
            (lambda (a b c regs)
                (setf (aref regs c) (logand (aref regs a) b))
                regs))
        (cons "borr"
            (lambda (a b c regs)
                (setf (aref regs c) (logior (aref regs a) (aref regs b)))
                regs))
        (cons "bori"
            (lambda (a b c regs)
                (setf (aref regs c) (logior (aref regs a) b))
                regs))
        (cons "setr"
            (lambda (a b c regs)
                (declare (ignore b))
                (setf (aref regs c) (aref regs a))
                regs))
        (cons "seti"
            (lambda (a b c regs)
                (declare (ignore b))
                (setf (aref regs c) a)
                regs))
        (cons "gtir"
            (lambda (a b c regs)
                (setf (aref regs c) (if (> a (aref regs b)) 1 0))
                regs))
        (cons "gtri"
            (lambda (a b c regs)
                (setf (aref regs c) (if (> (aref regs a) b) 1 0))
                regs))
        (cons "gtrr"
            (lambda (a b c regs)
                (setf (aref regs c) (if (> (aref regs a) (aref regs b)) 1 0))
                regs))
        (cons "eqir"
            (lambda (a b c regs)
                (setf (aref regs c) (if (= a (aref regs b)) 1 0))
                regs))
        (cons "eqri"
            (lambda (a b c regs)
                (setf (aref regs c) (if (= (aref regs a) b) 1 0))
                regs))
        (cons "eqrr"
            (lambda (a b c regs)
                (setf (aref regs c) (if (= (aref regs a) (aref regs b)) 1 0))
                regs))))
        ;(cons "#ip"
            ;(lambda (&rest rs)
                ;(declare (ignore rs))
                ;nil))))

;;; program logic ;;;

(defun run-vm (program ip-index regs)
    (loop
        while (< (aref regs ip-index) (length program))
        do (let* ((line (nth (aref regs ip-index) program))
                    (op (cdr (assoc (car line) opcodes :test #'string=))))
            (apply op (append (cdr line) (list regs)))
            ;(format t "~s ~s~%" line regs)
            (setf (aref regs ip-index) (1+ (aref regs ip-index)))))
    regs)

;;; main program ;;;

;; assume first line of instructions is #ip <index>
(let* ((instructions (mapcar #'parse-line (load-lines "input19.asm")))
       (ip-index     (cadar instructions)))
    (mapcar
        (lambda (initial-state)
            (format t "~a~%"
                (aref (run-vm (cdr instructions) ip-index initial-state) 0)))
        (list
            (vector 0 0 0 0 0 0)
            (vector 1 0 0 0 0 0))))

;; HEY ELVES! Your program was too slow, so I re-wrote it. It now takes under a
;; minute to run, instead of a decade (which a back-of-the-envelope calculation
;; suggests the running time would be). It's also ten lines shorter than the
;; original. Hope your hardware can handle negative numbers and 8-digit numbers.
;; (NOTE: If you use it with a square number, you'll need to subtract its square
;; root from the program's output to get your actual result.)

(quit)
