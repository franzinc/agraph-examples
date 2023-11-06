(require :agraph)

(in-package :triple-store-user)

(enable-!-reader)

(register-namespace "ex" "http://www.franz.com/simple#"
		    :errorp nil)

(defun fill-kennedy-db ()  
  (create-triple-store "test2"
                       :if-exists :supersede)
  (time
   (load-ntriples "kennedy.ntriples" :commit t)))

(fill-kennedy-db)

; Prolog as one retrieval language

(defun full-name (a b c)
  (format nil "~a ~a ~a"
	  (part->concise a)(part->concise b) (part->concise c)))

(<-- (full-name ?x ?fullname)
     (q- ?x !ex:first-name ?fn)
     (q- ?x !ex:middle-initial ?mn)
     (q- ?x !ex:last-name ?ln)
     (lisp ?fullname (full-name ?fn ?mn ?ln)))

(<-- (full-name ?x ?fn ?mn ?ln ?fullname)
     (q- ?x !ex:first-name ?fn)
     (q- ?x !ex:middle-initial ?mn)
     (q- ?x !ex:last-name ?ln)
     (lisp ?fullname (full-name ?fn ?mn ?ln)))

(<-- (male ?x)
     (q- ?x !ex:sex !ex:male))

(<-- (female ?x)
     (q- ?x !ex:sex !ex:female))

(<-- (father ?x ?y)
     (male ?x)
     (q- ?x !ex:has-child ?y))

(<-- (mother ?x ?y)
     (female ?x)
     (q- ?x !ex:has-child ?y))     

(<-- (parent ?x ?y)
     (father ?x ?y))

(<- (parent ?x ?y)
    (mother ?x ?y))
   
(<-- (grandparent ?x ?y)
     (parent ?x ?z)
     (parent ?z ?y))

(<-- (grandchild ?x ?y)
     (grandparent ?y ?x))

(<-- (ancestor ?x ?y)
     (parent ?x ?y))

(<-  (ancestor ?x ?y)     
     (parent ?x ?z)
     (ancestor ?z ?y))

(<-- (descendant ?x ?y)
     (ancestor ?y ?x))

(<-- (aunt ?x ?y) 
     (father ?z ?x)
     (female ?x)
     (father ?z ?w)
     (not (= ?x ?w))
     (parent ?w ?y))

(<-- (uncle ?x ?y) 
     (father ?z ?x)
     (male ?x)
     (father ?z ?w)
     (not (= ?x ?w))
     (parent ?w ?y))

(<-- (nephew ?x ?y)
     (aunt ?y ?x)
     (male ?x))

(<- (nephew ?x ?y)
    (uncle ?y ?x)
    (male ?x))

(<-- (niece ?x ?y)
     (aunt ?y ?x)
     (female ?x))

(<- (niece ?x ?y)
    (uncle ?y ?x)
    (female ?x))

(<-- (parent-child-have-same-name ?x ?y)     
     (q- ?x !ex:first-name ?n1)
     (parent ?x ?y)
     (q- ?y !ex:first-name ?n2)
     (= ?n1 ?n2))

(<-- (parent-child-went-to-ivy-league-school ?x ?y)     
     (q- ?x !ex:alma-mater ?am)
     (q- ?am !ex:ivy-league !ex:true)
     (parent ?x ?y)
     (q- ?y !ex:alma-mater ?am2)
     (q- ?am2 !ex:ivy-league !ex:true))

(<-- (parent-child-went-to-same-ivy-league-school ?x ?y)     
     (q- ?x !ex:alma-mater ?am)
     (q- ?am !ex:ivy-league !ex:true)
     (parent ?x ?y)
     (q- ?y !ex:alma-mater ?am))

(<-- (spouse ?x ?y)
     (q- ?x !ex:spouse ?y))

;; ?x has a spouse and children

(<-- (family ?x ?fam)
     (q- ?x !ex:spouse ?sp)
     (bagof ?ch (parent ?x ?ch) ?bag)
     (append ?bag (?sp) ?fam)
     \!)

;; ?x has no spouse ...

(<- (family ?x ?fam)
    (mother ?m ?x)
    (father ?f ?x)
    (siblings-incl ?m ?mlist)
    (siblings-incl ?f ?flist)
    (append  ?mlist ?flist ?fam))

(<-- (siblings-incl ?x ?siblist)
     (father ?father ?x)
     (bagof ?ch (father ?father ?ch) ?siblist))

(<- (siblings-incl ?x ?siblist)
    (append nil (?x) ?siblist))
    

;;;

#| queries

(select (?x ?y)
	(father ?x ?y))

(select (?x ?y)
	(aunt ?x ?y))

(select (?x ?y)
	(nephew ?x ?y))

|#
