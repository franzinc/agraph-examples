(in-package db.agraph.user)

#| not mentioned

in-degree, out-degree, nodal-degree
in-neighbors, out-neighbors, nodal-neighbors

group-degree-centrality
actor-closeness-centrality
group-closeness-centrality

clique (which also remains unimplemented)

|#

;;;;; Finally, some code to make some different sample networks

(defun build-florentine-family-graph ()
  "Silly but it works..."
  (make-tutorial-store)
  (let ((row 0)
	(names (make-array 16))
	(links nil)
	(namespace "http://www.franz.com/sna#"))
    (register-namespace "ffg" namespace)
    (db.agraph::map-lines 
   "
acciaiuoli    0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
albizzi       0 0 0 0 0 1 1 0 1 0 0 0 0 0 0 0
barbadori     0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0
bischeri      0 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0
castellani    0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0
ginori        0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
guadagni      0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1
lamberterschi 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
medici        1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1
pazzi         0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
peruzzi       0 0 0 1 1 0 0 0 0 0 0 0 0 0 1 0
pucci         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
ridolfi       0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 
salviati      0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0
strozzi       0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0
tornabuoni    0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 
" (lambda (line)
    (setf (aref names row) 
	  (resource (string+ namespace (read-from-string line))))
    (add-triple (aref names row) !rdf:type !ffg:family)
    (let ((position 14)
	  (linkp nil))
      (loop while 
	   (setf (values linkp position) 
		 (read-from-string line nil nil :start position))
	   for index from 0
	   when (= linkp 1) do
	   (push (cons row index) links)))
    (incf row)))
    (loop for (source . target) in links do
	 (add-triple (aref names source) 
		     !ffg:marriage
		     (aref names target)))))

#+(or)
(build-florentine-family-graph)

(defun ffg-marriage-generator (node)
  "Returns list of nodes to whom node is related by marriage" 
  ;; ugh!
  (union
   (collect-cursor (get-triples :s node :p !ffg:marriage)
		   :transform 'object)
   (collect-cursor (get-triples :p !ffg:marriage :o node)
		   :transform 'subject)
   :test 'upi=))

;;;;;;;

;;;;

(defun build-star-graph (size &key (create t))
  (let ((namespace (setup-for-sample-graphs create size)))
    (loop for i from 1 below size do
	 (add-triple 
	  (resource (string+ namespace i))
	  !sna:linksTo 
	  (resource (string+ namespace 0))))))

(defun build-circle-graph (size &key (create t))
  (let ((namespace (setup-for-sample-graphs create size)))
    (loop for i from 0 below size do
	 (add-triple (resource (string+ namespace (mod i size)))
		     !sna:linksTo 
		     (resource (string+ namespace (mod (1+ i) size)))))))

(defun build-line-graph (size &key (create t))
  (let ((namespace (setup-for-sample-graphs create size)))
    (loop for i from 0 to (- size 2) do
	 (add-triple (resource (string+ namespace i))
		     !sna:linksTo 
		     (resource (string+ namespace (1+ i)))))))

(defun linksTo-generator (node)
  (collect-cursor (get-triples :s node :p !sna:linksTo)
		  :transform 'object))

(defun linksFrom-generator (node)
  (collect-cursor (get-triples :s node :p !sna:linksFrom)
		  :transform 'object))

(defun linksToOrFrom-generator (node)
  (union
   (collect-cursor (get-triples :s node :p !sna:linksFrom)
		   :transform 'object)
   (collect-cursor (get-triples :s node :p !sna:linksTo)
		   :transform 'object)
   :test 'upi=))

(defun setup-for-sample-graphs (create? size)
  (when (or create? (not *db*))
    (make-tutorial-store))
  (let ((namespace "http://www.franz.com/sna#"))
    (register-namespace "sna" namespace)
    (add-triple !sna:linksTo !owl:inverseOf !sna:linksFrom)
    (loop for i from 0 below size do
	 (add-triple (resource (string+ namespace i)) !rdf:type !sna:node))
    (apply-rdfs++-reasoner)
    namespace))


;;; now we exercise it some

;; Graph density
(build-star-graph 7)
(density 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)

(build-circle-graph 7)
(density 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)

(build-line-graph 7)
(density 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)


;;; Actor degree centrality
;; star graph
(build-star-graph 7)
(setf group (select0-distinct ?s (q- ?s !rdf:type !sna:node)))
(actor-degree-centrality !sna:0 group 'linksTo-generator)
(actor-degree-centrality !sna:0 group 'linksFrom-generator)
(actor-degree-centrality !sna:1 group 'linksTo-generator)
(actor-degree-centrality !sna:1 group 'linksFrom-generator)

(build-star-graph 7)
(setf group (select0-distinct ?s (q- ?s !rdf:type !sna:node)))
(actor-degree-centrality !sna:1 group 'linksToOrFrom-generator)

;; circle graph

(build-circle-graph 7)
(setf group (select0-distinct ?s (q- ?s !rdf:type !sna:node)))
(actor-degree-centrality !sna:0 group 'linksTo-generator)
(actor-degree-centrality !sna:1 group 'linksToOrFrom-generator)


;;; ego network

;; start

(build-circle-graph 7)
(setf group (select0-distinct ?s (q- ?s !rdf:type !sna:node)))
(ego-group-select !sna:0 'linksTo-generator 0)
(ego-group-select !sna:0 'linksTo-generator 1)
(ego-group-select !sna:1 'linksToOrFrom-generator 2)
   
;; circle
(build-circle-graph 7)
(ego-group-select !sna:0 'linksTo-generator 0)
(ego-group-select !sna:0 'linksTo-generator 1)


;;; betweenness-centrality

;; star
(build-star-graph 7)
   
(actor-betweenness-centrality 
 !sna:0 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)

(let ((group (select0-distinct ?s (q- ?s !rdf:type !sna:node))))
  (mapcar (lambda (node)
	    (list node 
		  (actor-betweenness-centrality 
		   node group 'linksToOrFrom-generator)))
	  group))

;; line 

(build-line-graph 7)

(let ((group (select0-distinct ?s (q- ?s !rdf:type !sna:node))))
  (mapcar (lambda (node)
	    (list node 
		  (actor-betweenness-centrality 
		   node group 'linksToOrFrom-generator)))
	  group))


;;; group-betweenness-centrality
;; star
(build-star-graph 7)
(group-betweenness-centrality 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)

;; circle
(build-circle-graph 7)
(group-betweenness-centrality 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)

;; line
(build-line-graph 7)
(group-betweenness-centrality 
 (select0-distinct ?s (q- ?s !rdf:type !sna:node))
 'linksToOrFrom-generator)





