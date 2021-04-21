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
(defun applicable (tm typ st)
  (cond ((endp tm) nil)
        ((consp tm) (if (and (equal (first (first tm)) st)
                            (equal (second (first tm)) typ))
                     (first tm)
                     (applicable (rest tm) typ st)))))

;; finds the head creature, of power and toughness 2/2
(defun find-head (tape)
  (cond ((endp tape) nil)
        ((consp tape) (if (and (= 2 (third (first tape)))
                               (= 2 (fourth (first tape))))
                        (first tape)
                        (find-head (rest tape))))))

;; move the computation
;; return the new tape and a new state
(defun infest (tape st tm)
  (let* ((hd (find-head tape))
         (rlg (applicable tm (first hd) st)))
  (cons (remove-equal hd (append (list (third rlg)) tape)) (fourth rlg))))

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
  ;; step the tm by 1 step before deciding our next course of action
  (let* ((head (find-head tape)) ;; find the current head of the tape

         ;; compute the new tape by removing the head and adding a new one
         (new (infest tape st tm))

         ;; advance the tape by one step, 
         (advanced (snuffers (vigor-beam (first new) (second head)))))
    (cond 
      ;; we're done taking steps
      ((zp n) nil)
      ;; head is an assassin: we've halted
      ((equal (first head) 'assassin) tape)
      ;; otherwise, keep running the tape
      (t (mtgi (second new) advanced tm (- n 1))))))


;; order this mtg tape 
;; first, we need to define insert: by modelling
;; this similarly to insertion sort, we can prove some fairly
;; simple lemmas easily

;; defines the greater than relation on creatures
(defun gt-creature (c1 c2)
  (<= (fourth c1) (fourth c2)))

;; inserts a creature into a list of creatures
(defun insert-creature (creature l) 
  (cond
    ((endp l) (cons creature l))
    ((consp l) (if (gt-creature creature (first l))
                   (cons (first l) (insert-creature creature (rest l)))
                   (cons creature l)))))

;; sort a list of creatures
(defun isort-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
     (insert-creature (first creatures)
                      (isort-creatures (rest creatures))))))

;; we then need the different "pieces" of the tape, so we 
;; can model the pieces of the half tape

;; this gets us the left side of the half tape
;; we exclude the head, regardless of color
(defun white-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
      (if (and 
            (equal 'white (second (first creatures)))
            (not (equal 2 (fourth (first creatures)))))
          (cons (first creatures) (white-creatures (rest creatures)))
          (white-creatures (rest creatures))))))

;; this gets us the right side of the half tape
;; one minor difference, though: we get the head here as well
(defun green-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
      (if (or 
            (equal 'green (second (first creatures)))
            (equal 2 (fourth (first creatures))))
          (cons (first creatures) (white-creatures creatures))
          (white-creatures creatures)))))

;; we are now equipped to transform an arbitrary battlefield of creatures
;; into the structure of an mtg tape, preserving the ordering information for now
(defun battlefield-to-tape (creatures)
  (cons (isort-creatures (white-creatures creatures))
        (isort-creatures (green-creatures creatures))))

;; shows that some tape contains a card of a given 
;; Listof Creature Nat -> Boolean
(defun contains-toughness-and-color (creatures hp color)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (or (and
                            (equal hp (third (first creatures)))
                            (equal color (second (first creatures)))
                           (contains-toughness-and-color (rest creatures) hp color))))))

;; we need to prove some lemmas to show that we don't need mtgi anymore
;; notably, we will need to show a new function operating on creatures, 
;; mtgi-ord, will produce the same output as mtgi. interestingly, we don't care about the
;; values of the output aside from the symbols. once we **have** an ordered tape, we
;; can remove all relevant ordering information and operate on it as we wish.
;; but we'll still need to keep the ordering information: this presents an interesting challenge.
;; how can we show that mtgi-ord's output represents the same output as mtgi?

(defun ordered-creatures (creatures)
  (cond
    ((endp creatures) t)
    ((endp (rest creatures)) t)
    (t (and (gt-creature (second creatures) (first creatures))
            (ordered-creatures (rest creatures))))))

(defthm ordered-isort-creatures
  (implies
    (creaturesp creatures)
    (ordered-creatures (isort-creatures creatures))))

;; based on the acl2 insertion sort tutorial, we also need to prove that
;; insertion sort produces a permutation of the input, as it might 
;; always return nil (and still be ordered!)
;; to do so, we need to set up an inductive proof, since it's not quite
;; as trivial to admit for creatures as it is for integers. (why?)

(defthm insert-is-member
  (implies 
    (and
      (creaturep c)
      (creaturesp creatures))
    (mem c (insert-creature c creatures))))

(defthm isort-is-creaturesp
  (implies 
    (creaturesp creatures)
    (creaturesp (isort-creatures creatures))))

(defthm isort-preserves-first
  (implies
    (and
      (consp creatures)
      (creaturesp creatures)
      (creaturep (first creatures)))
    (mem (first creatures) (isort-creatures creatures))))

;; base case
(defthm isort-is-permutation-nil
  (implies
    (and
      (not (consp creatures))
      (creaturesp creatures))
    (permutation creatures (isort-creatures creatures))))

;; cons case
(defthm isort-is-permutation-cons
  (implies
    (and
      (consp creatures)
      (creaturesp creatures)
      ;; induction step
      (implies 
        (and
          (creaturesp creatures))
          (permutation (rest creatures) (del (first creatures) (isort-creatures creatures)))))
    (permutation creatures (isort-creatures creatures))))


;; determines if the creatures are arranged in progressive order: that is, every creature is either
;; - the head, or
;; - offset by 1 from the head, or
;; - has a card of 1 less toughness and matching power before it
(defun sequential (creatures)
  (cond
    ((endp creatures) t)
    ((endp (rest creatures)) t)
    (t (and
         (or (equal (third (first creatures)) (third (second creatures)))
             (equal (+ 1 (third (first creatures))) (third (second creatures))))
         (sequential (rest creatures))))))

;; a useful function to ensure we have properly ordered tapes
;; this works on all creatures of a given color (ie, green creatures
;; and white creatures, corresponding to the **unordered** half-tapes)
(defun well-formed-battlefield (creatures)
  (and
    (consp creatures)
    (creaturesp creatures)
    (contains-toughness (green-creatures creatures) 2)
    (not (contains-toughness (white-creatures creatures) 2))
    (sequential (isort-creatures (white-creatures creatures)))
    (sequential (isort-creatures (green-creatures creatures)))))


;; how to unite them into one proof? not sure..
;; either way, we need to fix up that induction proof.
;; for now, though, i'll accept it
; (defthm isort-is-permutation 
;   (implies
;     (and
;       (equal isort-is-permutation-nil t)
;       (equal isort-is-permutation-cons t)
;       (creaturesp creatures))
;     (permutation creatures (isort-creatures creatures))))

;; need to next show that some operations, once we resort the list, will remain true (ie the 2/2 thing)
(defthm head-exists-and-is-two-two
  (implies
      (well-formed-battlefield creatures)
    (= (third (find-head creatures)) 2)))

(defun mtgi* (st tape tm n)
  (let ((res (mtgi st tape tm n)))
    (cond
      ((endp res) nil)
      ((consp res) (cons (isort-creatures (green-creatures res))
                         (isort-creatures (white-creatures res)))))))

(defthm mtgi-is-well-formed
  (implies
    (and
      (booleanp st)
      (well-formed-battlefield creatures)
      (natp n)
      (> n 0))
    (if (mtgi st creatures *rogozhin-rotlungs* n)
        (well-formed-battlefield (mtgi st creatures *rogozhin-rotlungs* n))
        (endp (mtgi st creatures *rogozhin-rotlungs n)))))

;; prove that running mtgi will either result in a new head, or the tape is empty.
;; this is weird... it shouldn't be able to determine this?
(defthm new-head-exists
  (implies
    (and
      (booleanp st)
      (well-formed-battlefield creatures)
      (natp n)
      (> n 0))
    (or (endp (mtgi st creatures *rogozhin-rotlungs* n))
        (find-head (mtgi st creatures *rogozhin-rotlungs* n)))))



;; prove that running mtgi and ordering the input is a permutation -- not bad?
;; we already have the permutation, and are defining the ordered property ourselves


;; prove that mtgi-ord produces the same output as ordering after the fact


;; reduce the output down to just the symbols with a mapper
; (defun map-mtg (creatures)
;   (cond
;     ((endp creatures) nil)
;     ((consp creatures)))

;; prove that the symbols are isomorphic to the rogozhin alphabet

;; prove that mtgi-ord's output, once reduced, is equivalent to utmi.

;; then, need to have a function which actually operates on an ordered battlefield. would look a lot like utmi:

(defconst *rogozhin-assoc* 
  '((aetherborn 1)
    (basilisk 1>)
    (cephalid 1<)
    (demon 1>1)
    (elf 1<1)
    (faerie b)
    (giant b>)
    (harpy b<)
    (illusion b>1)
    (juggernaut b<1)
    (kavu b2)
    (leviathan b3)
    (myr c)
    (noggle c>)
    (orc c<)
    (pegasus c>1)
    (rhino c<1)
    (sliver c2)
    (assassin HALT)))

(defun map-creature (creature)
  (second (assoc (first creature) *rogozhin-assoc*)))

(defun map-tape (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (cons (map-creature (first creatures))
                             (map-tape (rest creatures))))))

; (defun mtgi-ord (st tape tm n)
;   (declare (xargs :measure (nfix n)))
;   (cond
;     ;; done taking steps
;     ((zp n) nil)
;     ;; symbol appears in the program
;     (())
;     ;; symbol we don't recognize: must be a halt!
;     (t tape)







