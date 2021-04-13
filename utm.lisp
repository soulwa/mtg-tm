;; an implementation of a Rogozhin (2,18) UTM

(defconst *alphabet*
  '(a b c d e f g h i j k l m n o p q r))
  
(defconst *even-huh-tag-sys*
  '(2 ((a ()) (b (b)) (c (b c))) (c c a a a a)))

(defun utm-symp (x)
  (member x *alphabet*))

;; listof symbol
;; not programmatically different from utm-tape- but this one
;; is used for production functions
(defun utm-losp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-losp (cdr x))))))

(defun remove-n (n x)
  (if (zp n)
      x
      (remove-n (- n 1) (cdr x))))

;; because rogozhin TMs "eat" tapes left to right
;; no need for a half-tape

;; a list of symbols
(defun utm-tapep (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-tapep (cdr x))))))

;; production function
;; a cons of a symp to a los-symp
(defun utm-funcp (x)
  (and (consp x)
       (= (length x) 2)
       (utm-symp (car x))
       (utm-losp (car (cdr x)))))

;; a list or "set" of prod-funcs
;; I don't use sets for now because they suck
(defun utm-funcsp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-funcp (car x))
                        (utm-funcsp (cdr x))))))

;; does this utm-func x apply to sym y
(defun utm-func-appliesp (x y)
  (and (utm-funcp x)
       (utm-symp y)
       (equal (car x) y)))

;; apply func
(defun utm-func-apply (x)
  (car (cdr x))) 

;; return the applicable func from a set
;; assumes such a func exists
(defun utm-funcs-applicable (x y)
  (cond ((endp x) nil)
        ((consp x) (if (utm-func-appliesp (car x) y)
                       (car x)
                       (utm-funcs-applicable (cdr x) y)))))

;; a tag system has a tape,
;; deletion number, and prod. function
;; Alphabet is included in the defintion of tape
;; '(,deletion-number ,prod-funcs ,tape)
(defun tag-sysp (x)
  (and (consp x)
       (equal (length x) 3)
       (natp (first x))
       (utm-funcsp (second x))
       (utm-tapep (third x))))

;; step the tape
;; find the applicable func in the tag system
;; using the first element of the tape and the set of production functions
;; append the application of that function to the end
;; delete the deletion number from the head
(defun utm-step (m funcs tape)
  (remove-n m (append tape
                      (utm-func-apply
               		  (utm-funcs-applicable funcs (car tape))))))

(defun utm-exec (m funcs tape n)
  (declare (xargs :measure (nfix n)))
  (cond
   ;; both of these are termination conditions
   ((< (length tape) m) tape)
   ((zp n) `(nil ,tape))
   (t (utm-exec m funcs (utm-step m funcs tape) (- n 1)))))

;; wrapper for utm-exec, which works on a tag system construct
(defun utm-tag-sys-exec (tag-sys n)
  (utm-exec (first tag-sys)
            (second tag-sys)
            (third tag-sys)
            n))

;; some more work needs to be done for the full utm:
;; tape will need to move left/right
;; inital tape setup specified by rogozhin's model?:

(defconst *rogozhin-alphabet*
  (1 1> 1< 1>1 1<1 b b> b< b>1 b<1 b2 b3 c c> c< c>1 c<1 c2))

(defconst *rogozhin-states*
  '(q1 q2))

(defconst *utm-2-18-prog*
  '((q1
      (1 c2 L q1)
      (1> 1<1 R q1)
      (1< c2 L q1)
      (1>1 1 R q1)
      (1<1 1>1 L q1)
      (b b< R q1)
      (b> b<1 R q1)
      (b< b L q1)
      (b>1 b R q1)
      (b<1 b>1 L q1)
      (b2 b3 L q2)
      (b3 b>1 L q2)
      (c 1> L q2)
      (c> c< R q1)
      (c< c>1 L q1)
      (c>1 c<1 L q1)
      (c<1 HALT nil nil)
      (c2 1< R q1))
    (q2
      (1 1< R q2)
      (1> 1< R q2)
      (1< 1> L q2)
      (1>1 1<1 R q2)
      (1<1 1 L q2)
      (b b2 R q1)
      (b> b< R q2)
      (b< b> L q2)
      (b>1 b<1 R q2)
      (b<1 b> L q2)
      (b2 b R q1)
      (b3 b<1 R q2)
      (c c< R q2)
      (c> c< R q2)
      (c< c> L q2)
      (c>1 c2 R q2)
      (c<1 c2 L q1)
      (c2 c L q2))))

(defun utm-symbolp (x)
  (or (member x *rogozhin-alphabet*) (equal x 'HALT)))

(defun utm-dirp (x)
  (or (equal x 'L) (equal x 'R) (equal x nil)))

(defun utm-statep (x)
  (or (member x *rogozhin-states*) (equal x nil)))

(defun utm-instrp (x)
  (and
    (tlp x)
    (= (length x) 4)
    (utm-symbolp (first x))
    (utm-symbolp (second x))
    (utm-dirp (third x))
    (utm-statep (fourth x))))

(defun utm-loinstrp (x)
  (cond
    ((endp x) t))
    ((consp x) (and (utm-instrp (first x))
                    (utm-loinstrp (rest x)))))

(defun utm-progp (x)
  (and
    (tlp x)
    (= (length x) 2)
    (utm-statep (first x))
    (utm-loinstrp (second x))))

;; we *will* need to check specifically for the 2-18 machine, as that's the
;; only valid utm-- might change our theorems somewhat?

;; get the next instruction for a utm, given a symbol and instructions
(defun instr-sym (sym instrs)
  (cond
    ((endp progm) nil) ;; error case
    ((equal sym (first (first instrs))) (first instrs))
    (t (instr sym (rest instrs)))))

;; get the next instruction for a utm, given a symbol, state, and a program
(defun instr (sym state progm)
  (cond
    ((endp progm) nil) ;; HALT symbol
    ((equal state (first (first progm))) (instr-sym (sym (first progm))))
    (t (instr (sym state (rest progm))))))

;; definition of tape based on tmi-reductions.lisp

;; gets the head of the tape, or a new blank symbol
(defun sym (x)
  (if (consp x) (first x) '1<))

(defun half-tapep (x)
  (cond
    ((endp x) nil)
    ((consp x) (and (utm-symbolp x)
                    (half-tapep x)))))

(defun tapep (x)
  (and (consp x)
    (half-tapep (first x))
    (half-tapep (rest x))))

(defun show-tape (tape)
  (cond ((consp tape)
         (reverse (car tape)
                  (cons '[ (cons (sym (cdr tape)) (cons '] (cdr (cdr tape)))))))
        (t nil)))

(defun tape-head (tape) 
  (sym (rest tape)))

(defun new-tape (mark dir tape)
  ;; shadow tape with the new tape, since we always replace the mark
  (let ((tape (cons (first tape) (cons mark (rest (rest tape))))))
    (case dir
      ;; push head to the beginning of (first tape), which
      ;; represents the order of the symbols backwards
      (L (cons (rest (first tape))
               (cons (sym (first tape))
                     (rest tape))))
      ;; add new symbol to second half of tape
      (R (cons (cons (sym (rest tape))
                     (first tape))
               (rest (rest tape)))))))


(defun utmi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (cond 
    ;; we're done taking steps
    ((zp n) nil) 
    ;; the symbol appears in the progm
    ((instr (tape-head tape) st tm) 
      (let ((inst (instr (tape-head tape)) st tm))
      (utmi (fourth inst)
            (new-tape (second inst) (third inst) tape)
            tm
            (- n 1))))
    ;; the symbol doesn't appear: must be HALT symbol, as it doesn't have lookup
    ;; not sure if halting will be provable here, if the HALT symbol is included
    ;; in utm-symbolp
    (t nil)))



