(require :uiop)

;; define everything
(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))
(defvar *inputpathname* "")
(defvar *inputlines* nil)
(defvar *outputlines* nil)
; make a list of all instruction types, may expand this list to include opcodes for each
(defparameter *instructiontypes* (list (cons 'rtype '("add" "addu" "and" "jr" "nor" "or" "slt" "sltu" "sll" "srl" "sub" "subu"))
                                       (cons 'itype '("addi" "addiu" "andi" "beq" "bne" "lbu" "lhu" "ll" "lui" "lw" "ori" "slti" "sltiu" "sb" "sc" "sh" "sw"))
                                       (cons 'jtype '("j" "jal"))))

(defun readinputfile (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf *inputlines* (push
                                              (remove "" (uiop:split-string (string-left-trim *whitespaces* (substitute #\Space #\, line)) :separator " ") :test #'string=) *inputlines*)))
      (close in)))
  (delete NIL *inputlines*)
  (setf *inputlines* (reverse *inputlines*)))

(defun processinputlist (inputlist)
  (loop for x in inputlist
        with types = *instructiontypes*
        when (equal #\: (char (reverse (car x)) 0))
          collect (cons x 'label)
        when (member (car x) (car types) :test #'string=)
          collect (cons x 'rtype)
        when (member (car x) (car (cdr types)) :test #'string=)
          collect (cons x 'itype)
        when (member (car x) (car (cdr (cdr types))) :test #'string=)
          collect (cons x 'itype)))

(defun pass1 (input)
  ; since pseudo-instructions aren't part of the mips core instruction set, we (probably?) don't have to deal with them.
  ; if that functionality were to be added, it would be before the labels are processed
  ;
  ; the output of this function is a list of two lists,
  ; where the first is the instructions to be processed (minus the labels),
  ; and the second is the 'symbol table', where the labels are associated with a memory address in decimal
  (loop for x in input
        for y from 0
        if (equal (cdr x) 'label)
          collect (cons x y) into symboltable
        else
          collect x into instructions
        finally (return (list instructions symboltable))))

(defun pass2 (input)
  ; convert the input to machine code.
  ; the input is a list of two lists, where the first is the instructions, and the second is the 'symbol table'
  ;
  ; need special cases for instructions with labels (beq and bne come to mind) to convert labels.
  ; otherwise, probably as simple as looking up the machine code translation
  ; in a defined parameter or file, and simply outputting it.
  )

(defun machinecode->file (input outfile)
  ; finally, place the decoded machine code into a specified output file
  )

;; script portion
(if (uiop:file-exists-p (car (uiop:command-line-arguments)))
    (setf *inputpathname* (pathname (car (uiop:command-line-arguments)))))

(if (eq (uiop:file-exists-p *inputpathname*) nil)
    (progn
      (print "provide a valid .s file")
      (exit :CODE 1)))

(readinputfile (car (uiop:command-line-arguments)))

;(print *inputlines*)
;(print (processinputlist *inputlines*))
(print (pass1 (processinputlist *inputlines*)))
