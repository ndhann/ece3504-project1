(require :uiop)

;; define everything
(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))
(defvar *inputpathname* "")
(defvar *inputlines* nil)
(defvar *outputlines* nil)
(defparameter *instructiontypes* (list (cons 'rtype '("add" "addu" "and" "jr" "nor" "or" "slt" "sltu" "sll" "srl" "sub" "subu"))
                                     (cons 'itype '("addi" "addiu" "andi" "beq" "bne" "lbu" "lhu" "ll" "lui" "lw" "ori" "slti" "sltiu" "sb" "sc" "sh" "sw"))
                                     (cons 'jtype '("j" "jal"))))

(defun readinputfile (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf *inputlines* (push
                                            (uiop:split-string (string-left-trim *whitespaces* line) :separator " ")
                                            *inputlines*)))
      (close in)))
  (delete NIL *inputlines*)
  (setf *inputlines* (reverse *inputlines*)))

(defun processinputlist (inputlist)
  (loop for x in inputlist
        with types = *instructiontypes*
        when (member (car x) (car types) :test #'string=)
          collect (cons x 'rtype)
        when (member (car x) (car (cdr types)) :test #'string=)
          collect (cons x 'itype)
        when (member (car x) (car (cdr (cdr types))) :test #'string=)
          collect (cons x 'itype)))

;; script portion
(if (uiop:file-exists-p (car (uiop:command-line-arguments)))
    (setf *inputpathname* (pathname (car (uiop:command-line-arguments)))))

(if (eq (uiop:file-exists-p *inputpathname*) nil)
    (progn
      (print "provide a valid .s file")
      (exit :CODE 1)))

(readinputfile (car (uiop:command-line-arguments)))

;(print *inputlines*)
(print (processinputlist *inputlines*))
