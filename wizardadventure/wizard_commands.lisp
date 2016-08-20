(defun look ()
    (append (describe-location *location* *nodes*)
            (describe-paths *location* *edges*)
            (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
    (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
        (if next
            (progn (setf *location* (car next))
                   (look))
            '(you cannot go that way.))))

(defun pickup (object)
    (cond ((member object
               (objects-at *location* *objects* *object-locations*))
           (push (list object 'body) *object-locations*)
           `(you are now carrying the ,object))
          (t '(you cannot get that.))))

(defun inventory ()
    (cons 'items- (objects-at 'body *objects* *object-locations*)))
