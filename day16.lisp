;; run this with "sbcl --load <this>"
(require :cl-ppcre)

;;; input handling functions ;;;

(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))


(defun make-probe (before-line instruction-line after-line)
    ;; each probe is an a-list:
    ;;  (
    ;;      (before . #(1 3 1 3))
    ;;      (instruction . #(14 0 3 0)
    ;;      (after  . #(0 3 1 3))
    ;;  )
    (pairlis
        (list
            'before
            'instruction
            'after)
        (mapcar (lambda (line)
            (apply #'vector
                (mapcar
                    #'parse-integer
                    (cl-ppcre:all-matches-as-strings "\\d+" line))))
            (list
                before-line
                instruction-line
                after-line))))

(defun parse-instructions (lines)
    (mapcar (lambda (line)
        (apply #'vector
            (mapcar
                #'parse-integer
                (cl-ppcre:all-matches-as-strings "\\d+" line))))
        lines))

(defun parse-probes (lines probes)
    (if (string= (car lines) "")
        ;; we're up to the instruction part
        (cons
            probes
            (parse-instructions (cddr lines)))
        ;; the first four lines are a probe
        (parse-probes
            (cddddr lines)
            (cons
                (make-probe (car lines) (cadr lines) (caddr lines))
                probes))))

(defun parse-lines (lines)
    ;; returns (probes . instructions)
    (parse-probes lines nil))

;;; opcodes ;;;

(defvar opcodes
    ;; each opcode is a function that takes four arguments (a b c regs)
    ;; where 'regs is a vector of 4 items.
    ;; Mutates the vector and returns it.
    (list
        (cons 'addr
            (lambda (a b c regs)
                (setf (aref regs c) (+ (aref regs a) (aref regs b)))
                regs))
        (cons 'addi
            (lambda (a b c regs)
                (setf (aref regs c) (+ (aref regs a) b))
                regs))
        (cons 'mulr
            (lambda (a b c regs)
                (setf (aref regs c) (* (aref regs a) (aref regs b)))
                regs))
        (cons 'muli
            (lambda (a b c regs)
                (setf (aref regs c) (* (aref regs a) b))
                regs))
        (cons 'banr
            (lambda (a b c regs)
                (setf (aref regs c) (logand (aref regs a) (aref regs b)))
                regs))
        (cons 'bani
            (lambda (a b c regs)
                (setf (aref regs c) (logand (aref regs a) b))
                regs))
        (cons 'borr
            (lambda (a b c regs)
                (setf (aref regs c) (logior (aref regs a) (aref regs b)))
                regs))
        (cons 'bori
            (lambda (a b c regs)
                (setf (aref regs c) (logior (aref regs a) b))
                regs))
        (cons 'setr
            (lambda (a b c regs)
                (declare (ignore b))
                (setf (aref regs c) (aref regs a))
                regs))
        (cons 'seti
            (lambda (a b c regs)
                (declare (ignore b))
                (setf (aref regs c) a)
                regs))
        (cons 'gtir
            (lambda (a b c regs)
                (setf (aref regs c) (if (> a (aref regs b)) 1 0))
                regs))
        (cons 'gtri
            (lambda (a b c regs)
                (setf (aref regs c) (if (> (aref regs a) b) 1 0))
                regs))
        (cons 'gtrr
            (lambda (a b c regs)
                (setf (aref regs c) (if (> (aref regs a) (aref regs b)) 1 0))
                regs))
        (cons 'eqir
            (lambda (a b c regs)
                (setf (aref regs c) (if (= a (aref regs b)) 1 0))
                regs))
        (cons 'eqri
            (lambda (a b c regs)
                (setf (aref regs c) (if (= (aref regs a) b) 1 0))
                regs))
        (cons 'eqrr
            (lambda (a b c regs)
                (setf (aref regs c) (if (= (aref regs a) (aref regs b)) 1 0))
                regs))))

;;; program logic ;;;

(defun possible-opcodes (probe)
    ;; returns a cons pair (numeric-opcode . list of possible opcodes)
    (let ((before      (cdr (assoc 'before      probe)))
          (instruction (coerce   (cdr (assoc 'instruction probe)) 'list))
          (after       (cdr (assoc 'after       probe))))
    (cons
        (car instruction)
        (mapcar #'car
            (remove-if-not
                (lambda (pair)
                    (let ((tmp (copy-seq before)))
                        (apply (cdr pair) (append (cdr instruction) (list tmp)))
                    (equalp tmp after)))
            opcodes)))))

(defun shrink-opcode-table (raw-alist)
    (loop for i from 0 to 15 collect (assoc i raw-alist)))

(defun narrow-it-down (remaining-opcodes knowns)
    ;; given this opcode table:
    ;; ((0 'opcode-a)
    ;;  (1 'opcode-a 'opcode-b)
    ;;  (2 'opcode-a 'opcode-c))
    ;; removes 'opcode-a from all other lists
    ;; and repeats
    (if (null remaining-opcodes)
        knowns
        (let ((new-knowns
                (remove-if-not
                    (lambda (entry) (= (length entry) 2))
                    remaining-opcodes))
              (new-remaining
                (remove-if
                    (lambda (entry) (= (length entry) 2))
                    remaining-opcodes))
              (known-names
                (mapcar #'cadr knowns)))
            (narrow-it-down
                (mapcar
                    (lambda (entry)
                        (cons
                            (car entry)
                            (remove-if
                                (lambda (op) (member op known-names))
                                (cdr entry))))
                    new-remaining)
                (append new-knowns knowns)))))

(defun solve-opcodes (probes)
    (narrow-it-down
        (shrink-opcode-table
            (reduce
                (lambda (table pos-ops)
                    (acons
                        (car pos-ops)
                        (intersection
                            (cdr pos-ops)
                            (cdr (assoc (car pos-ops) table)))
                        table))
                (mapcar #'possible-opcodes probes)
                :initial-value
                (loop
                    for i from 0 to 15
                    collect (cons i (mapcar #'car opcodes)))))
        nil))

(defun run-vm (opcode-table instructions)
    (let ((regs (vector 0 0 0 0)))
        (loop
            for instruction-vector in instructions
            do (let ((instruction (coerce instruction-vector 'list)))
                (apply
                    ;; get opcode function
                    (cdr (assoc
                            ;; get opcode name
                            (cadr (assoc (car instruction) opcode-table))
                            opcodes))
                    (append (cdr instruction) (list regs)))))
        regs))

;;; main program ;;;

(defvar input-data (parse-lines (load-lines "input16.txt")))
(defvar probes (car input-data))
(defvar instructions (cdr input-data))
;; part 1
(format t "~a~%"
    (count-if
        (lambda (probe)
            (>= (length (cdr (possible-opcodes probe))) 3))
        probes))
;; part 2
(format t "~a~%"
    (aref (run-vm (solve-opcodes probes) instructions) 0))

(quit)
