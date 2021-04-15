;; An ACL2 implementation of an MTGTM.
;; https://arxiv.org/abs/1904.09828  

;; TODO: better variable names
;; MTGTM creature types.
(defconst *types*
          '(aetherborn basilisk cephalid demon elf faerie giant harpy illusion
                       juggernaut kavu leviathan myr noggle orc pegasus rhino
                       silver))

;; MTGTM tape-creature colors
(defconst *tokencolor*
          '(white green))

;; MTGTM creature type recognizer
(defun ctypep (x)
  (member x *types*))

;; MTGTM creature color recognizer
(defun ccolorp (x)
  (member x *tokencolor*))

;; Example tape-creature
(defconst *aetherborn-ex*
  '(aetherborn green 2 2))

;; example rotlung
(defconst *rotlung-1<q1*
  '(T 'cephalid *aetherborn-ex* T))

;; example xathrid 
;; note that (rotlungp *xathrid-b2q1*) will be true, since
;; they both produce a creature, just that xathrid produces a tapped
;; creature (transitioning the state)
(defconst *xathrid-b2q1*
  '(nil kavu (leviathan white 2 2) nil))

;; creature cards (or tokens) are of a type, have a color,
;; and have power and toughness
(defun creaturep (x)
  (and (= (length x) 4)
       (ctypep (first x))
       (ccolorp (second x))
       (natp (third x))
       (natp (fourth x))))

;; a collection of creatures, where order does not matter
(defun creaturesp (x)
  (cond ((endp x) T)
        ((consp x) (and (creaturep (first x))
                       (creaturesp (rest x))))))

;; rotlung renanimator- writes symbols when infest causes
;; the head to die 
;; two booleans represent state
(defun rotlungp (x)
  (and (= (length x) 4)
       (booleanp (first x))
       (ctypep (second x))
       (creaturep (third x))
       (booleanp (fourth x))))

;; list of rotlungs
(defun rotlungsp (x)
  (cond ((endp x) T)
        ((consp x) (and (rotlungp (first x))
                        (rotlungsp (rest x))))))

;; find the rotlung appropriate rotlung given state and type
(defun applicable (tm typ st)
  (cond ((endp tm) 'error)
        ((consp tm) (if (and (equal (first (first tm)) st)
                            (equal (second (first tm)) typ))
                     (first tm)
                     (applicable (rest tm) typ st)))))

;; finds the head creature, of power and toughness 2/2
(defun find-head (tape)
  (cond ((endp tape) 'error)
        ((consp tape) (if (and (= 2 (third (first tape)))
                               (= 2 (fourth (first tape))))
                        (first tape)
                        (find-head (rest tape))))))

;; move the computation
;; return the new tape and a new state
(defun infest (tape st tm)
  (let* ((hd (find-head tape))
        (rlg (applicable tm (first hd) st)))
  (cons (remove hd (append (list rlg) tape)) (fourth rlg))))

;; cast beam on creatures
(defun vigor-beam (tape color)
  (cond ((endp tape) nil)
        ((consp tape) (if (equal color (second (first tape)))
                     (cons (list (first (first tape))
                                 (second (first tape))
                                 (+ 2 (third (first tape)))
                                 (+ 2 (fourth (first tape))))
                           (vigor-beam (rest tape) color))
                     (cons (first tape) (vigor-beam (rest tape) color))))))
                   
;; termination condition
(defun victoryp (tape1 tape2)
  (equal tape1 tape2))

;; cast snuffers, dealing -1 -1 to all
(defun snuffers (tape)
  (cond ((endp tape) nil)
        ((consp tape) (cons (list (first (first tape))
                               (second (first tape))
                               (- (third (first tape)) 1)
                               (- (fourth (first tape)) 1))
                         (snuffers (rest tape))))))


;; interpret this MTGTM for n steps or until termination, whichever comes first
(defun mtgi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (let* ((head (find-head tape))
        (new (infest tape st tm))
        (advanced (vigor-beam (first new) (second head))))
    (cond ((zp n) nil)
          ((victoryp tape advanced) nil)
          (T
            (mtgi (second new)
                  advanced
                  tm
                  (- n 1))))))
