;; An ACL2 implementation of an MTGTM.
;; https://arxiv.org/abs/1904.09828  

;; determines if x is a member of l
(defun mem (x l)
  (consp (member x l)))

;; removes an element x from a list l
(defun del (x l)
  (cond
    ((endp l) nil)
    ((consp l) (if (equal x (first l))
                   (rest l)
                   (cons (first l) (del x (rest l)))))))

(defun permutation (l1 l2)
  (cond 
    ((endp l1) (endp l2))
    ((consp l1) (and (mem (first l1) l2)
                     (permutation (rest l1) (del (first l1) l2))))))

;; MTGTM creature types.
(defconst *types*
          '(aetherborn basilisk cephalid demon elf faerie giant harpy illusion
                       juggernaut kavu leviathan myr noggle orc pegasus rhino
                       sliver assassin))

;; MTGTM tape-creature colors
(defconst *tokencolor*
          '(white green blue))

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
  `(T cephalid ,*aetherborn-ex* T))

;; example xathrid 
;; note that (rotlungp *xathrid-b2q1*) will be true, since
;; they both produce a creature, just that xathrid produces a tapped
;; creature (transitioning the state)
(defconst *xathrid-b2q1*
  '(nil kavu (leviathan white 2 2) nil))

(defconst *rogozhin-rotlungs*
  ;; state | death proc | new head | new state
  '((t aetherborn (sliver white 2 2)     t)
    (t basilisk   (elf green 2 2)        t)
    (t cephalid   (sliver white 2 2)     t)
    (t demon      (aetherborn green 2 2) t)
    (t elf        (demon white 2 2)      t)
    (t faerie     (harpy green 2 2)      t)
    (t giant      (juggernaut green 2 2) t)
    (t harpy      (faerie white 2 2)     t)
    (t illusion   (faerie green 2 2)     t)
    (t juggernaut (illusion white 2 2)   t)
    (t kavu       (leviathan white 2 2)  nil)
    (t leviathan  (illusion white 2 2)   nil)
    (t myr        (basilisk white 2 2)   nil)
    (t noggle     (orc green 2 2)        t)
    (t orc        (pegasus white 2 2)    t)
    (t pegasus    (rhino green 2 2)      nil)
    (t rhino      (assassin blue 2 2)    t) ;; HALT
    (t sliver     (cephalid green 2 2)   t)

    (nil aetherborn (cephalid green 2 2)   nil)
    (nil basilisk   (cephalid green 2 2)   nil)
    (nil cephalid   (basilisk white 2 2)   nil)
    (nil demon      (elf green 2 2)        nil)
    (nil elf        (aetherborn white 2 2) nil)
    (nil faerie     (kavu green 2 2)       t)
    (nil giant      (harpy green 2 2)      nil)
    (nil harpy      (giant white 2 2)      nil)
    (nil illusion   (juggernaut green 2 2) nil)
    (nil juggernaut (giant white 2 2)      nil)
    (nil kavu       (faerie green 2 2)     t)
    (nil leviathan  (juggernaut green 2 2) nil)
    (nil myr        (orc green 2 2)        nil)
    (nil noggle     (orc green 2 2)        nil)
    (nil orc        (noggle white 2 2)     nil)
    (nil pegasus    (sliver green 2 2)     nil)
    (nil rhino      (sliver white 2 2)     t)
    (nil sliver     (myr white 2 2)        nil)))


;; creature cards (or tokens) are of a type, have a color,
;; and have power and toughness
(defun creaturep (x)
  (and (= (length x) 4)
       (ctypep (first x))
       (ccolorp (second x))
       (natp (third x))
       (>= 2 (third x))
       (natp (fourth x))
       (>= 2 (fourth x))
       (= (third x) (fourth x))))

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
(defun find-rotlung (tm typ st)
  (cond ((endp tm) nil)
        ((consp tm) (if (and (equal (first (first tm)) st)
                            (equal (second (first tm)) typ))
                     (first tm)
                     (find-rotlung (rest tm) typ st)))))

;; finds the head creature, of power and toughness 2/2
(defun find-head (tape)
  (cond ((endp tape) nil)
        ((consp tape) (if (and (= 2 (third (first tape)))
                               (= 2 (fourth (first tape))))
                        (first tape)
                        (find-head (rest tape))))))

;; we need to extract both the new state and the new creature
;; while we have rlg
(defun new-head-state (head tm st)
  (let* ((typ (first head))
        (rlg (find-rotlung tm typ st))
        (new-creature (third rlg))
        (new-state (fourth rlg)))
    (cons new-creature new-state)))

;; simple remove
(defun infest (head tape)
  (remove-equal head tape))
    
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
                   


;; cast snuffers, dealing -1 -1 to all
(defun snuffers (tape)
  (cond ((endp tape) nil)
        ((consp tape) (cons (list (first (first tape))
                               (second (first tape))
                               (- (third (first tape)) 1)
                               (- (fourth (first tape)) 1))
                         (snuffers (rest tape))))))

;; combine vigor, beam, and snuffers to move in the desired direction
(defun move (tape color)
  (snuffers (vigor-beam tape color)))

(defun new-tape (new-head head tape)
  (let* ((color (second new-head))
         (headless (infest head tape)))
    (case color
      (white (cons new-head (move headless color)))
      (green (cons new-head (move headless color)))
      (t tape))))

(defun mtgi (st tape tm n)
  (let* ((head (find-head tape))
         (typ (first head))
         (rlg (find-rotlung tm typ st))
         (new-head (third rlg))
         (new-state (fourth rlg)))
    (cond 
      ((zp n) tape)
      (t (mtgi new-state (new-tape new-head head tape) tm (- n 1))))))

(defconst *example-mtg-tape*
  '(  (faerie green 3 3)
      (aetherborn green 4 4)
      (faerie green 5 5)
      (aetherborn green 6 6)
      (aetherborn green 7 7)
      (faerie green 8 8)
      (faerie green 9 9)
      (aetherborn green 10 10)
      (aetherborn green 11 11)
      (aetherborn green 12 12)
      (aetherborn green 13 13)
      (aetherborn green 14 14)
      (aetherborn green 15 15)
      (aetherborn green 16 16)
      (aetherborn green 17 17)
      (aetherborn green 18 18)
      (aetherborn green 19 19)
      (aetherborn green 20 20)
      (faerie green 21 21)
      (aetherborn green 22 22)
      (aetherborn green 23 23)
      (aetherborn green 24 24)
      (aetherborn green 25 25)
      (aetherborn green 26 26)
      (faerie green 27 27)
      (faerie green 28 28)
      (aetherborn green 29 29)
      (faerie green 30 30)
      (aetherborn green 31 31)
      (aetherborn green 32 32)
      (aetherborn green 33 33)
      (aetherborn green 34 34)
      (aetherborn green 35 35)
      (aetherborn green 36 36)
      (aetherborn green 37 37)
      (aetherborn green 38 38)
      (aetherborn green 39 39)
      (aetherborn green 40 40)
      (aetherborn green 41 41)
      (aetherborn green 42 42)
      (faerie green 43 43)
      (aetherborn green 44 44)
      (aetherborn green 45 45)
      (aetherborn green 46 46)
      (aetherborn green 47 47)
      (aetherborn green 48 48)
      (aetherborn green 49 49)
      (aetherborn green 50 50)
      (aetherborn green 51 51)
      (faerie green 52 52)
      (faerie green 53 53)
      (rhino green 54 54)
      (rhino green 55 55)

      (faerie white 2 2)
      (aetherborn white 3 3)
      (myr white 4 4)
      (aetherborn white 5 5)
      (aetherborn white 6 6)
      (aetherborn white 7 7)
      (aetherborn white 8 8)
      (myr white 9 9)
      (aetherborn white 10 10)
      (aetherborn white 11 11)
      (aetherborn white 12 12)
      (aetherborn white 13 13)
      (aetherborn white 14 14)
      (aetherborn white 15 15)
      (aetherborn white 16 16)
      (myr white 17 17) ))

;; TODO: Add some correctness checking


