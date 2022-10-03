(require :uiop)
;(require :split-sequence)
(defvar inputpathname "")
(defvar inputlines nil)
(if (uiop:file-exists-p (car (uiop:command-line-arguments)))
    (setf inputpathname (pathname (car (uiop:command-line-arguments)))))

(if (eq (uiop:file-exists-p inputpathname) nil)
    (progn
      (print "provide a valid .s file")
      (exit :CODE 1)))
(let ((in (open (car (uiop:command-line-arguments)) :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (setf inputlines (push (uiop:split-string line :separator " ") inputlines)))
    (close in)))

(setf inputlines (reverse inputlines))
(print inputlines)
