;; Freetext indexing tuturial for lisp users..
;; The idea is that all the following lisp forms are evaluated
;; from top to bottom, one at a time.

(require :agraph)                    ; load Agraph

(in-package :triple-store-user)      ; go in the right package

(enable-!-reader)  

(enable-print-decoded t)             ; make printing look nicer

(create-triple-store "ag-test" :if-exists :supersede)

;; Tell AllegroGraph on which predicates you want to index.

(create-freetext-index "fti-tutorial"
                       :predicates '(!<http://www.w3.org/2000/01/rdf-schema#comment>
                                     !<http://www.w3.org/2000/01/rdf-schema#label>))

;; Add some triples:

(add-triple !"Jans" !rdfs:comment !"Born in Amsterdam in the Netherlands")
(add-triple !"Gary" !rdfs:comment !"Born in Springfield in the USA")
(add-triple !"Steve" !rdfs:label !"Born in New Amsterdam in the USA")

;; Using the index:

(freetext-get-unique-subjects '(match "amsterd*") :index "fti-tutorial")

;=> ({Steve} {Jans})

(freetext-get-unique-subjects '(and "amsterdam" "usa")) ; a boolean expression

(freetext-get-unique-subjects "\"New Amsterdam\"")      ; a phrase, note the quotes

(freetext-get-triples '(and "usa" "born"))  ; return a cursor

;=> #<db.agraph.cursor::transform-cursor @ #x1003166c42>

;; Oops, this is a cursor. So lets work with this cursor. 
;; First bind it to cursor..

(setf cursor (freetext-get-triples '(and "usa" "born")))

;; then loop over the cursor with the handy iterate-cursor macro or
;; map-cursor function.

(iterate-cursor (triple cursor)
    (print triple))

(map-cursor 10 'print (freetext-get-triples '(and "usa" "born")))

;; And sometimes you only want the subjects back, especially in your favorite 
;; query language: 

(freetext-get-unique-subjects '(and "netherlands" "born"))

;=> ({"Jans"})


;;; ====== so now a bigger example. 

(register-namespace "o" "http://www.franz.com/simple#")

;; First we add some new triples to our open triple-store, note that the
;; object of each new triples is a long string filled with random numbers
;; (in english)

(defun fill-dummy-ntriple-file-and-load-it (max)
  (let ((list '("one " "two " "three " "four "
		"five " "six " "seven " "eight " "nine " "ten ")))
    (with-open-file (out "dum.ntriples" :direction :output
		     :if-exists :supersede)
      (dotimes (i max)
	(let ((subject (string+ '<subject- i "> ")))
	  (dotimes (j 5)
	    (let ((predicate "<http://www.w3.org/2000/01/rdf-schema#comment> ")
		  (object (apply 'triple-store::string+
				 (let ((li nil))
				   (dotimes (i (1+ (random 8)))
				     (push (nth (random 10) list) li))
				   li))))
	      (format out "~a~a~s .~%" subject predicate object))))))
    (load-ntriples "dum.ntriples")))

(fill-dummy-ntriple-file-and-load-it 10)
	      
;; Let's look at the triples

(dolist (e (get-triples-list))
  (print e))

;; So now we want to play with this file: let us write a little test
;; function:

(defun test (arg)
  (map-cursor 50 (lambda (a) (print a))
              (freetext-get-triples arg)))

;; simple expressions

(test "eight")

(test '(and "ten" "eight"))

(test '(and "ten" "eight" (or "three" "four")))

(test '(or (and "five" "one")
	(and "ten" "eight" (or "three" "four"))))

;; wildcards -> * is zero or more occurrences
;;              ? is one character
;; no * allowed in phrases

(test '(match "?i*")) ; five six nine

(test '(match "?i?e")) ; five nine

(test '(or (and (match "fiv*") (match "on*"))
        (and (match "te*") (match "eigh*")
         (or (match "th*ree") (match "fo*ur") (match "\"one five\"")))))


;; And here is an example of a large file, filled with weapon systems,
;; terrorists, a a lot of common knowledge from the cyc database
;; (available on request: please mail ssears@franz.com)

;; We include this non-trivial example but it will allow us to do some
;; select queries


(defun read-gov ()
  (format t "~%Add triples")
  (load-ntriples "/s/ja/allegrograph-examplefiles/Gov.ntriples")
  (format t "~%Commit triples")
  (commit-triple-store))

(time (read-gov))

(register-namespace "c" "http://www.cyc.com/2002/04/08/cyc#")

(get-triples-list :p !rdfs:comment)

(test '(and "collection" "people"))

(test '(phrase "collection of people"))

;; use it in prolog

(select (?person)
  (lisp ?list (freetext-get-unique-subjects '(and "collection" "people")))
  (member ?person ?list)
  (q- ?person !rdfs:subClassOf !c:AsianCitizenOrSubject))

(select (?person)
  (lisp ?list (freetext-get-unique-subjects '(phrase "collection of people")))
  (member ?person ?list)
  (q- ?person !rdfs:subClassOf !c:AsianCitizenOrSubject))

(close-triple-store)

(open-triple-store "ag-test")

(select (?person)
  (lisp ?list (freetext-get-unique-subjects '(and "collection" "people")))
  (member ?person ?list)
  (q- ?person !rdfs:subClassOf !c:AsianCitizenOrSubject))
