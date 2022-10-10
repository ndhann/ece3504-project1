(require :uiop)

;; define everything
(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))
(defvar *inputpathname* "")
(defvar *inputlines* nil)
(defvar *outputlines* nil)
; make a list of all instruction types
(defparameter *instructiontypes* (list (cons 'rtype '("add" "addu" "and" "jr" "nor" "or" "slt" "sltu" "sll" "srl" "sub" "subu"))
                                       (cons 'itype '("addi" "addiu" "andi" "beq" "bne" "lbu" "lhu" "ll" "lui" "lw" "ori" "slti" "sltiu" "sb" "sc" "sh" "sw"))
                                       (cons 'jtype '("j" "jal"))))
; map instructions to opcodes and function codes
(defparameter *opcodemap* '((add (#X0 #X20)) (addu (#X0 #X21)) (and (#X0 #X24)) (jr (#X0 #X08)) (nor (#X0 #X27)) (or (#X0 #X25)) (slt (#X0 #X2A)) (sltu (#X0 #X2B)) (sll (#X0 #X00)) (srl (#X0 #X02)) (sub (#X0 #X22)) (subu (#X0 #X23))
                            (addi #X8) (addiu #X9) (andi #XC) (beq #X4) (bne #X5) (lbu #X24) (lhu #X25) (ll #X30) (lui #XF) (lw #X23) (ori #XD) (slti #XA) (sltiu #XB) (sb #X28) (sc #X38) (sh #X29) (sw #X2B)
                            (j #X2) (jal #X3)))
(defparameter *registermap* '(($zero 0) ($at 1) ($v0 2) ($v1 3) ($a0 4) ($a1 5) ($a2 6) ($a3 7)
                              ($t0 8) ($t1 9) ($t2 10) ($t3 11) ($t4 12) ($t5 13) ($t6 14) ($t7 15)
                              ($s0 16) ($s1 17) ($s2 18) ($s3 19) ($s4 20) ($s5 21) ($s6 22) ($s7 23)
                              ($t8 24) ($t9 25) ($k0 26) ($k1 27) ($gp 28) ($sp 29) ($fp 30) ($ra 31)))

(defun readinputfile (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf *inputlines*
                                (push
                                 (remove "" (uiop:split-string (string-left-trim *whitespaces* (substitute #\Space #\, line)) :separator " ") :test #'string=) *inputlines*)))
      (close in)))
  (delete NIL *inputlines*)
  (setf *inputlines* (reverse *inputlines*)))

(defun processinputlist (inputlist)
  (loop for x in inputlist
        for y from 1
        with types = *instructiontypes*
        if (equal #\: (char (reverse (car x)) 0))
        collect (cons x 'label)
        else if (member (car x) (cdr (car types)) :test #'string=)
        collect (cons x 'rtype)
        else if (member (car x) (cdr (car (cdr types))) :test #'string=)
        collect (cons x 'itype)
        else if (member (car x) (cdr (car (cdr (cdr types)))) :test #'string=)
        collect (cons x 'jtype)
        else
        do (progn (print (concatenate 'string "Undefined function " (car x) " found on line " (write-to-string y) ", aborting"))
                  (exit :CODE 1))))

(defun pass1 (input)
; since pseudo-instructions aren't part of the mips core instruction set, we (probably?) don't have to deal with them.
; if that functionality were to be added, it would be before the labels are processed
;
; the output of this function is a list of two lists,
; where the first is the instructions to be processed (minus the labels),
; and the second is the 'symbol table', where the labels are associated with a memory address in decimal
  (loop for x in input
        for y from 1
        if (equal (cdr x) 'label)
          collect (cons (read-from-string (remove #\: (car (car x)))) y) into symboltable
          and do (setf y (- y 1)) ;make sure that subsequent label-line mappings aren't offset
        else
          collect x into instructions
        finally (return (list instructions symboltable))))

(defun rtype->machinecode (x)
  ; converts the input list to machine code for an rtype instruction

  (cond ((or (string= (car (car x)) "sll") (string= (car (car x)) "srl")) ; check for shifts, and translate them accordingly
         (list
          ;opcode
          (car (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*))))
          ;rs
          0
          ;rt
          (car (cdr (assoc (read-from-string (car (cdr (cdr (car x))))) *registermap*)))
          ;rd
          (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*)))
          ;sh
          (read-from-string (car (cdr (cdr (cdr (car x))))))
          ;func
          (car (cdr (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))))))
        (t ; catch-all case, for everything other than a shift
         (list
          ;opcode
          (car (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*))))
          ;rs
          (car (cdr (assoc (read-from-string (car (cdr (cdr (car x))))) *registermap*)))
          ;rt
          (car (cdr (assoc (read-from-string (car (cdr (cdr (cdr (car x)))))) *registermap*)))
          ;rd
          (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*)))
          ;sh
          0
          ;func
          (car (cdr (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))))))))

(defun itype->machinecode (x pos symboltable)
    (cond ((or (string= (car (car x)) "lb") (string= (car (car x)) "lbu") (string= (car (car x)) "lh") (string= (car (car x)) "lhu")
                                            (string= (car (car x)) "lw") (string= (car (car x)) "sb") (string= (car (car x)) "sh")
                                            (string= (car (car x)) "sw") (string= (car (car x)) "ll") (string= (car (car x)) "sc"))
         (list
          ;opcode
          (format nil "~6,'0b" (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*))))
          ;rs
          (format nil "~5,'0b" (car (cdr (assoc (read-from-string (remove #\) (car (cdr (uiop:split-string (car (cdr (cdr (car x)))) :separator "("))))) *registermap*))))
          ;rt
          (format nil "~5,'0b" (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*))))
          ;imm
          (format nil "~16,'0b" (read-from-string (car (uiop:split-string (car (cdr (cdr (car x)))) :separator "("))))
          ))
        ((string= (car (car x)) "lui")
         (list
          ;opcode
          (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))
          ;rs
          0
          ;rt
          (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*)))
          ;imm
          (read-from-string (car (cdr (cdr (car x)))))
          ))
        ((or (string= (car (car x)) "beq") (string= (car (car x)) "bne"))
         (list
          ;opcode
          (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))
          ;rs
          (car (cdr (assoc (read-from-string (car (cdr (cdr (car x))))) *registermap*)))
          ;rt
          (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*)))
          ;imm, converted from a label to an offset
          (- (cdr (assoc (read-from-string (car (cdr (cdr (cdr (car x)))))) symboltable)) pos)
          ))
        (t
         (list
          ;opcode
          (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))
          ;rs
          (car (cdr (assoc (read-from-string (car (cdr (cdr (car x))))) *registermap*)))
          ;rt
          (car (cdr (assoc (read-from-string (car (cdr (car x)))) *registermap*)))
          ;imm
          (read-from-string (car (cdr (cdr (cdr (car x)))))))))
  )

(defun jtype->machinecode (x)
  (list
   ;opcode
   (car (cdr (assoc (read-from-string (car (car x))) *opcodemap*)))
   ;address
   ;TODO: might need some logic to determine whether it's hexadecimal or not
   (read-from-string (car (cdr (car x))))
   ))

(defun pass2 (input)
; convert the input to machine code.
; the input is a list of two lists, where the first is the instructions, and the second is the 'symbol table'
;
; need special cases for instructions with labels (beq and bne come to mind) to convert labels.
; otherwise, probably as simple as looking up the machine code translation
; in a defined parameter or file, and simply outputting it.

; loop over the instructions
  (loop for x in (car input)
        for y from 2 ; since next-pc is relative to the next instruction
        when (eq (cdr x) 'rtype)
          collect (rtype->machinecode x)
        when (eq (cdr x) 'itype)
          ; make sure to pass the symboltable here so labels can be defined as offsets
          collect (itype->machinecode x y (car (cdr input)))
        when (eq (cdr x) 'jtype)
          collect (jtype->machinecode x)))

(defun machinecode->file (input outfile)
; finally, place the decoded machine code into a specified output file
  (loop for x in input
        collect (format nil "~{~A~}" x)))

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
(print (pass1 (processinputlist *inputlines*)))
(print (pass2 (pass1 (processinputlist *inputlines*))))
(format nil (format nil "~~~D,'0b ~~~D,'0b" 8 4) 1 3)
