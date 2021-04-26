;; an implementation of a UTM(2,18) tag system
;; experimentation- not part of utmi
(include-book "acl2s/check-equal" :dir :system)

;; some possible tags
(defconst *alphabet*
  '(a b c d e f g h i j k l m n o p q r))
  
;; an example of a tag system program
(defconst *even-huh-tag-sys*
  '(2 ((a ()) (b (b)) (c (b c))) (c c a a a a)))

;; determines if valid tag
;; if an invalid tag has been produced, it's likely
;; HALT
(defun tag-symp (x)
  (member x *alphabet*))

;; listof symbol
;; used for production functions
(defun tag-losp (x)
  (cond ((endp x) T)
        ((consp x) (and (tag-symp (car x))
                        (tag-losp (cdr x))))))

;; remove n elements of a list from the list
(defun remove-n (n x)
  (if (zp n)
      x
      (remove-n (- n 1) (cdr x))))

;; because tag systems "eat" tapes left to right
;; no need for a half-tape

;; a list of symbols
(defun tag-tapep (x)
  (cond ((endp x) T)
        ((consp x) (and (tag-symp (car x))
                        (tag-tapep (cdr x))))))

;; production function
;; a cons of a symp to a los-symp
(defun tag-funcp (x)
  (and (consp x)
       (= (length x) 2)
       (tag-symp (car x))
       (tag-losp (car (cdr x)))))

;; a list or "set" of prod-funcs
(defun tag-funcsp (x)
  (cond ((endp x) T)
        ((consp x) (and (tag-funcp (car x))
                        (tag-funcsp (cdr x))))))

;; does this tag-func x apply to sym y
(defun tag-func-appliesp (x y)
  (and (tag-funcp x)
       (tag-symp y)
       (equal (car x) y)))

;; apply func
(defun tag-func-apply (x)
  (car (cdr x))) 

;; return the applicable func from a set
;; assumes such a func exists
(defun tag-funcs-applicable (x y)
  (cond ((endp x) nil)
        ((consp x) (if (tag-func-appliesp (car x) y)
                       (car x)
                       (tag-funcs-applicable (cdr x) y)))))

;; a tag system has a tape,
;; deletion number, and prod. function
;; Alphabet is included in the defintion of tape
;; '(,deletion-number ,prod-funcs ,tape)
(defun tag-sysp (x)
  (and (consp x)
       (equal (length x) 3)
       (natp (first x))
       (tag-funcsp (second x))
       (tag-tapep (third x))))

;; step the tape
;; find the applicable func in the tag system
;; using the first element of the tape and the set of production functions
;; append the application of that function to the end
;; delete the deletion number from the head
(defun tag-step (m funcs tape)
  (remove-n m (append tape
                      (tag-func-apply
               		  (tag-funcs-applicable funcs (car tape))))))

(defun tag-exec (m funcs tape n)
  (declare (xargs :measure (nfix n)))
  (cond
   ;; both of these are termination conditions
   ((< (length tape) m) tape)
   ((zp n) `(nil ,tape))
   (t (tag-exec m funcs (tag-step m funcs tape) (- n 1)))))

;; wrapper for utm-exec, which works on a tag system construct
(defun tag-sys-exec (tag-sys n)
  (tag-exec (first tag-sys)
            (second tag-sys)
            (third tag-sys)
            n))


;; implementation of Rogozhin's UTM(2, 18)
(defconst *rogozhin-alphabet*
  '(1 1> 1< 1>1 1<1 b b> b< b>1 b<1 b2 b3 c c> c< c>1 c<1 c2))

(defconst *rogozhin-states*
  '(q1 q2))

(defconst *utm-2-18*
  '((q1 1   c2  L q1)
    (q1 1>  1<1 R q1)
    (q1 1<  c2  L q1)
    (q1 1>1 1   R q1)
    (q1 1<1 1>1 L q1)
    (q1 b   b<  R q1)
    (q1 b>  b<1 R q1)
    (q1 b<  b   L q1)
    (q1 b>1 b   R q1)
    (q1 b<1 b>1 L q1)
    (q1 b2  b3  L q2)
    (q1 b3  b>1 L q2)
    (q1 c   1>  L q2)
    (q1 c>  c<  R q1)
    (q1 c<  c>1 L q1)
    (q1 c>1 c<1 R q2)
    (q1 c<1 HALT nil nil)
    (q1 c2  1<  R q1)

    (q2 1   1<  R q2)
    (q2 1>  1<  R q2)
    (q2 1<  1>  L q2)
    (q2 1>1 1<1 R q2)
    (q2 1<1 1   L q2)
    (q2 b   b2  R q1)
    (q2 b>  b<  R q2)
    (q2 b<  b>  L q2)
    (q2 b>1 b<1 R q2)
    (q2 b<1 b>  L q2)
    (q2 b2  b   R q1)
    (q2 b3  b<1 R q2)
    (q2 c   c<  R q2)
    (q2 c>  c<  R q2)
    (q2 c<  c>  L q2)
    (q2 c>1 c2  R q2)
    (q2 c<1 c2  L q1)
    (q2 c2  c   L q2)))

;; represents a symbol that's part of a UTM(2, 18)
(defun utm-symbolp (x)
  (member x *rogozhin-alphabet*))

(ACL2s::check= (not (utm-symbolp 'c2)) nil)
(ACL2s::check= (utm-symbolp 'z) nil)

;; represents a direction to move the tape
(defun utm-dirp (x)
  (or (equal x 'L) (equal x 'R) (equal x nil)))

(ACL2s::check= (utm-dirp 'L) t)
(ACL2s::check= (utm-dirp 'R) t)
(ACL2s::check= (utm-dirp 'up) nil)

;; represents a state in the UTM(2, 18)
(defun utm-statep (x)
  (or (member x *rogozhin-states*) (equal x nil)))

(ACL2s::check= (not (utm-statep 'q1)) nil)
(ACL2s::check= (not (utm-statep 'q2)) nil)
(ACL2s::check= (utm-statep 'q) nil)

;; represents an instruction
(defun utm-instrp (x)
  (and
    (true-listp x)
    (= (length x) 5)
    (utm-statep (first x))
    (utm-symbolp (second x))
    (or (utm-symbolp (third x)) (equal x 'HALT))
    (utm-dirp (fourth x))
    (utm-statep (fifth x))))

(ACL2s::check= (not (utm-instrp (car *utm-2-18*))) nil)
(ACL2s::check= (utm-instrp '(q1 q2 q3 q4)) nil)

;; represents a list of instructions, or: a machine
;; might not be turing complete (no guarantees are made
;; by this function), but a program which runs on a tape
;; we know our *utm-2-18* is complete thanks to Rogozhin's proof
(defun utm-loinstrp (x)
  (cond
    ((endp x) t)
    ((consp x) (and (utm-instrp (first x))
                    (utm-loinstrp (rest x))))))

(ACL2s::check= (utm-loinstrp (cons '(q1 q2 q3 q4) *utm-2-18*)) nil)

;; gets the head of the tape, or a new blank symbol
(defun sym (x)
  (if (consp x) (first x) '1<))

(ACL2s::check= (sym '(1 c)) '1)
(ACL2s::check= (sym '()) '1<)

;; get the next instruction for a utm, given a symbol, state, and a program
(defun instr (sym st tm)
  (cond
    ((endp tm) nil) ;; error
    ((and
      (equal st (first (first tm)))
      (equal sym (second (first tm))))
     (first tm))
    (t (instr sym st (rest tm)))))
    
(ACL2s::check= (instr 'c 'q1 *utm-2-18*) '(q1 c 1> L q2))

;; definition of tape based on tmi-reductions.lisp

;; represents half of a turing machine's tape
(defun half-tapep (x)
  (cond
    ((endp x) t)
    ((consp x) (and (utm-symbolp (first x))
                    (half-tapep (rest x))))))
                    
(ACL2s::check= (not (half-tapep '(c c c))) nil)
(ACL2s::check= (half-tapep '(1 2 3)) nil)

;; represents a full tape: 
;; '(,first-half ,second-half)
;; where first-half is reversed from its current order, and
;; (first second-half) is the head of the tape
(defun tapep (x)
  (and (consp x)
    (half-tapep (first x))
    (half-tapep (rest x))))
    
(ACL2s::check= (not (tapep '((c c c) (c c c)))) nil)

;; rev1 based on tmi-reductions.lisp
;; this function adds each element of x to a in reverse order
;; so it returns
;; (append (reverse x) a)
(defun rev1 (x a)
  (cond
    ((endp x) a)
    ((consp x) (rev1 (rest x) (cons (first x) a)))))

;; shows the tape (from tmi-reductions.lisp)
;; for debugging purposes
(defun show-tape (tape)
  (cond ((consp tape)
         (rev1 (car tape)
                  (cons '[ (cons (sym (cdr tape)) (cons '] (cdr (cdr tape)))))))
        (t nil)))
        
(ACL2s::check= (show-tape '( (c2 1 b) (b3 b2 c)))
               '(b 1 c2 [ b3 ] b2 c))
                    

;; gets the head of the tape
(defun tape-head (tape) 
  (sym (second tape)))

;; generates a new tape given a new mark to write
;; and a direction to move in
(defun new-tape (mark dir tape)
  ;; shadow tape with the new tape, since we always replace the mark
  (let ((tape (cons (first tape) (list (cons mark (rest (second tape)))))))
    (case dir
      ;; push head to the beginning of (first tape), which
      ;; represents the order of the symbols backwards
      (L (cons (rest (first tape))
               (list
                (cons (sym (first tape))
                      (second tape)))))
      ;; add new symbol to second half of tape
      (R (cons (cons (sym (second tape))
                     (first tape))
               (list (rest (second tape)))))
      ;; don't do anything (halted)
      (t tape))))

;; runs a UTM on a given tape
(defun utmi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (cond 
    ;; we're done taking steps
    ((zp n) nil) 
    ;; the symbol appears in the progm
    ((instr (tape-head tape) st tm) 
     (let ((inst (instr (tape-head tape) st tm)))
       (utmi (fifth inst)
             (new-tape (third inst) (fourth inst) tape)
             tm
             (- n 1))))
    ;; the symbol doesn't appear: must be HALT symbol, as it doesn't have lookup
    (t tape)))

(defconst *example-tape* 
  '((b 1 b 1 1 b b 1 1 1 1 1 1 1 1 1 1 1 b 1 1 1 1 1 b b 1 b 1 1 1 1 1 1 1 1 1 1 1 1 b 1 1 1 1 1 1 1 1 b b c<1 c<1)
    (b 1 c 1 1 1 1 c 1 1 1 1 1 1 1 c)))

(defthm utmi-demo
  (and
    (equal
      (utmi 'q1 *example-tape* *utm-2-18* 6002)
      nil)
    (equal
      (utmi 'q1 *example-tape* *utm-2-18* 6003)
      '(nil (HALT C2 B> B> 1> 1> 1> 1> 1> 1> 1> 1> B> 1> 1> 1> 1>
        1> 1> 1> 1> 1> 1> 1> 1> B> 1> B> B> 1> 1> 1> 1> 1> B> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> B> B> 1> 1> B> 1> B> B> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1 1 1 1 1 1 1 C 1 C 1 C)))
    (equal
      (utmi 'q1 *example-tape* *utm-2-18* 100000)
      '(nil (HALT C2 B> B> 1> 1> 1> 1> 1> 1> 1> 1> B> 1> 1> 1> 1>
        1> 1> 1> 1> 1> 1> 1> 1> B> 1> B> B> 1> 1> 1> 1> 1> B> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> B> B> 1> 1> B> 1> B> B> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 
        1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1> 1 1 1 1 1 1 1 C 1 C 1 C)))))

(defun haltedp (tape)
  (equal (tape-head tape) 'HALT))

;; everything up to here submits

;; will likely need additional lemmas
;; refer to moore's paper for these?
(defthm utmi-stays-terminated
  (implies
    (and
      (utm-statep st)
      (tapep tape)
      (natp n)
      (natp m)
      (haltedp (utmi st tape *utm-2-18* n)))
    (haltedp (utmi st tape *utm-2-18* (+ m n)))))
