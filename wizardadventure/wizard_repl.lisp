(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun game-read ()
    (let ((cmd (read-from-string
                   (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun tweak-text (1st caps lit)
    (when 1st
        (let ((item (car 1st))
              (rest (cdr 1st)))
            (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
                  ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
                  ((eql item #\") (tweak-text rest caps (not lit)))
                  (lit (cons item (tweak-text rest nil lit)))
                  (caps (cons (char-upcase item) (tweak-text rest nil lit)))
                  (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (1st)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                                    (prin1-to-string 1st))
                                       'list)
                               t
                               nil)
                   'string))
    (fresh-line))

(defun game-repl ()
    (progn (princ "#> ")
        (let ((cmd (game-read)))
            (unless (eq (car cmd) 'quit)
                (game-print (game-eval cmd))
                (game-repl)))))
