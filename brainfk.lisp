; A Brainf**k interpreter.

(defparameter *ptr* 0)
(defparameter *mem* nil)

(defun init ()
    (setq *ptr* 0)
    (setq *mem* (make-array 256 :initial-element 0))
)

(defun inc-ptr ()
    (if (/= *ptr* 255)
        (setq *ptr* (1+ *ptr*))
        (progn (format t "Out of memory.") (exit))
    )
)

(defun dec-ptr ()
    (if (/= *ptr* 0)
        (setq *ptr* (1- *ptr*))        
    )
)

(defun inc-val ()
    (let ((val (aref *mem* *ptr*)))
        (case val
            (255       (setf (aref *mem* *ptr*) 0))
            (otherwise (setf (aref *mem* *ptr*) (1+ val)))
        )
    )
)

(defun dec-val ()
    (let ((val (aref *mem* *ptr*)))
        (case val
            (0         (setf (aref *mem* *ptr*) 255))
            (otherwise (setf (aref *mem* *ptr*) (1- val)))
        )
    )
)

(defun print-val ()
    (let ((val (aref *mem* *ptr*)))
        (format t "~c" (code-char val))
    )
)

(defun while (progs)
    (let ((val (aref *mem* *ptr*)))
        (if (/= val 0)
            (progn (eval progs) (while progs))
        )
    )
)

(defun parse-source (src)
    (map
        'list
        (lambda (c)
            (case c
                (#\> "(inc-ptr)")
                (#\< "(dec-ptr)")
                (#\+ "(inc-val)")
                (#\- "(dec-val)")
                (#\. "(print-val)")
                (#\[ "(while '(progn")
                (#\] "))")
            )
        )
        src
    )
)

(defun evaluate-bf (progs)
    (eval (read-from-string (format nil "(progn ~{~a~^ ~})" progs)))
)

(defun interpreter ()
    (format t "> ")
    (finish-output)
    (let ((src (read-line)))
        (if (not (equalp src "quit"))
            (let ((programs (parse-source src)))
                (init)
                (evaluate-bf programs)
                (terpri)
                (finish-output)
                (interpreter)
            )
            (progn (format t "Quit.~%") (exit))
        )
    )
)

(defun main () (interpreter))

(main)
