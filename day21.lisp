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

(defun run-vm-until (program ip-index regs halt-test)
    ;; halt-test should be an argument that accepts two args: the registers and
    ;; an instruction.
    ;; It should return a non-NIL value if the vm should halt.
    (loop
        while (< (aref regs ip-index) (length program))
        do (let* ((line (nth (aref regs ip-index) program))
                    (op (cdr (assoc (car line) opcodes :test #'string=))))
            (apply op (append (cdr line) (list regs)))
            ;(format t "~s ~s~%" line regs)
            (if (funcall halt-test regs line)
                (return regs))
            (setf (aref regs ip-index) (1+ (aref regs ip-index)))))
    regs)

;;; main program ;;;

;; assume first line of instructions is #ip <index>
(let* ((instructions (mapcar #'parse-line (load-lines "input21.asm")))
       (ip-index     (cadar instructions))
       (seen-values  (make-hash-table))
       (last-seen 0))
    ;; part 1 (makes big assumptions about the input!)
    (format t "~a~%"
        (aref
            (run-vm-until
                (cdr instructions)
                ip-index
                (vector 0 0 0 0 0 0)
                (lambda (regs line)
                    (declare (ignore regs))
                    (and
                        (string= (car line) "eqrr")
                        (= (cadr line) 1)
                        (= (caddr line) 0))))
            1))
    ;; part 2... is this going to go thru all values < 2^24?
    ;; takes about 18 minutes
    (run-vm-until
        (cdr instructions)
        ip-index
        (vector 0 0 0 0 0 0)
        (lambda (regs line)
            (if (and (string= (car line) "eqrr")
                        (= (cadr line) 1)
                        (= (caddr line) 0))
                (if (null (gethash (aref regs 1) seen-values))
                    (progn
                        (setf (gethash (aref regs 1) seen-values) t)
                        (setf last-seen (aref regs 1))
                        nil)
                    t)
                nil)))
    (format t "~a~%" last-seen))

(quit)

