(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
                (eq (cadr (assoc obj obj-locs)) loc)))
        (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
    (labels ((describe-obj (obj)
                  `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

