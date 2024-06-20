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

(defun parse-source (src)
    (map
        'list
        (lambda (c)
            (case c
                (#\> '(inc-ptr))
                (#\< '(dec-ptr))
                (#\+ '(inc-val))
                (#\- '(dec-val))
                (#\. '(print-val))
            )
        )
        src
    )
)

(defun main ()
    (init)
    (let ((src (read-line)))
        (let ((programs (parse-source src)))
            (map 'nil 'eval programs)
        )
    )
)

(main)
