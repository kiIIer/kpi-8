;;; task2-5.lisp
;;; -----------
;;; Finds the longest path in a directed graph without repeating nodes.

(defparameter *graph*
  '((a b c)
    (b c)
    (c d)
    (d)))

(defun neighbors (node graph)
  (cdr (assoc node graph)))

(defun all-paths-from (node graph visited)
  (let ((paths nil))
    (dolist (nbr (neighbors node graph) paths)
      (unless (member nbr visited)
        (dolist (subpath (all-paths-from nbr graph (cons nbr visited)))
          (push (cons node subpath) paths))))
    (push (list node) paths)
    paths))

(defun find-longest-path (graph)
  (let ((longest '()))
    (dolist (entry graph)
      (let ((start (car entry)))
        (dolist (path (all-paths-from start graph (list start)))
          (when (> (length path) (length longest))
            (setf longest path)))))
  longest))

(format t "~%Graph: ~A~%" *graph*)
(format t "Longest path: ~A~%" (find-longest-path *graph*))
(format t "~%")
