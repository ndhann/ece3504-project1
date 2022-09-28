(require :uiop)
(if (eq (uiop:file-exists-p (car (uiop:command-line-arguments))) nil)
    (progn
      (print "provide a valid filename")
      (exit :CODE 1)))
(let ((in (open (car (uiop:command-line-arguments)) :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "~a~%" line))
    (close in)))
