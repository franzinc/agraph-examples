;;
;; Generate a database of weather data.
;;
;; Generate a set of weather stations, and for each station hourly observations
;;
;; This database is well suited for distributed AllegroGraph if you use the graph
;; part of the quads to select the shard in which the quad will be placed
;;
;; For a given number of stations we always generate the exact same quads.
;; This is important as it allows us to run benchmarks comparing the
;; performance for different repository configurations.
;;
;; to use:
;; step 1:
;;   Start lisp
;;   load agraph.fasl (from the allegrograph client software distribution).
;;   compile and load this file
;;
;; step 2: 
;;   
;;   create database
;; 
;;     select a database name (default "weather")
;;     the number of stations of data to create.  (default 100)
;;
;;   (build-db :dbname "choosename" :stations 200)
;; 
;;   The database named will be created (and overwritten if it already exists).
;;   If the database name is declared to be a distributed repository 
;;   (via a declaration in agcluster.cfg) then a distributed repository will
;;   be created and the quads created by this code distributed to the shards.
;;
;;   The database name you specify is a 'repo-spec' which can express more
;;   then simply a database name
;; 
;;   e.g 
;;     "weather" means connect to an AllegroGraph server on the current machine
;;               on port 10035
;;     "61111/weather"  means connect to an AllegroGraph server on the current
;;               machine on port 61111
;;     "myname:mypass@sample.com:2312/weather"  means connect to machine "sample.com"
;;               on port 2312 using user name "myname"  and password "mypass"
;;
;;   build-db takes a :blank-nodes-p argument as well.  Normally the repo will
;;   be built using blank nodes to link observations together.  This is space
;;   efficient but files with lots of blank nodes are slower to load 
;;   from a serialized form due to the need to ensure that there 
;;   is just one mapping from blank node name
;;   to whatever object is chosen a runtime to represent that blank node.
;;   If you specify 
;;      :blank-nodes-p nil
;;   then resources will be used in place of blank nodes making the respository
;;   easier to load from a serialized copy if it.
;;
;;   Another argument to build-db is :attributes-p
;;   The default value is nil but if given a true value it will add an attribute
;;   to each quad thus allowing for the testing of loading and exporting of
;;   quads with attributes.  The attribute is "name" whose value is the name
;;   of the station.
;;
;;  step 3:
;;   do queries on the database.
;;   
;;   You can open the database using Webview and issue queries.
;;   We have some sample queries in this file.  
;;   See "Queries" at the end of this file for directions on how to 
;;   invoke the queries from Lisp.
;; 
;; 
;;
(defpackage :cl-user (:use :db.agraph))

(in-package :cl-user)

;; the quads generated
;;
;; wx:st_<stationname>  rdf:type      wx:station       wx:st_<stationname>
;; wx:st_<stationname>  rdf:label    "stationname"     wx:st_<stationname>
;; wx:st_<stationname>  wx:location   lat-long         wx:st_<stationname>
;; _bn2                 rdf:type      wx:observation   wx:st_<stationname>
;; _bn2                 wx:time       "2018-05-07T18:00:00Z"^^xsd:dateTime  wx:st_<stationname>
;; _bn2                 wx:temp       "20"^^xsd:integer wx:st_<stationname>
;; _bn2                 wx:wx         wx:rain          wx:st_<stationname>      [[if rain]]
;; _bn2                 wx:precip     "5"              wx_st:_<stationname>      [[if rain]]
;  _bn2                 wx:wx         ws:clear         wx:st_<stationname>      [[if no rain]]




(eval-when (compile load eval) (enable-!-reader))

(defparameter *dbname* "weather")

;; The range over which there will be observations made
(defparameter *time-min* (encode-universal-time   0 0 0 1 5 2018 0))
(defparameter *time-max*   (+ *time-min* (* 7 24 60 60))) ; 7 days in seconds

;; Make one observation per hour
(defparameter *sample-interval* (* 60 60)) ; one hour

;; Assign a location to each weather station with the lat and long
;; ranging from 0 to the max value specified here
(defparameter  *lat-max* 20.0d0)
(defparameter *long-max* 10.d00)

(defparameter *pct-rain* 0.30) ;; 30% chance of showers

(defparameter *wx-namespace* "http://demo.com/wx#")
(defparameter *lat-long-nd-ordinates* 
    '((:name :lat :minimum -90.0d0 :maximum 90.0d0 
             :resolution 1.0d-4 :type :latitude 
             :strip-width 0.001d0)
      (:name :lon :minimum -180.0d0 :maximum 180.0d0 
       :resolution 1.0d-4 :type :longitude)))

(defparameter *lat-long-nd-subtype* nil)



;; we'll use the same seed for our random number generator in order to
;; generate the same quads.
(defvar *wx-random-state*)
(defconstant *wx-random-seed* 2342417)

(defvar *blank-node-counter*)


;; coldest at 7am, hottest at 3pm

;; In order to give somewhat realistic temperatures we'll
;; vary the temperature between the given min and max
;; over the day by figuring that the temperature is
;; lowest at 7am and warmest at 3pm
;;
;;  8 hours of heating from 7am (0) to 3pm (1)
;; 16 hours of cooling from 3pm (1) to 7am (0)
(defparameter *temp-factors*
    ;; store a table of numbers between 0 and 1 which
    ;; is 0 at 4am and 1 and 3pm and evenly spaced
    ;; in between
    (let ((res (make-array 24 :initial-element 0)))
      (do ((i 0 (1+ i)))
          ((>= i 24))
        (if* (<= i 7)
           then (setf (svref res i) (* (- 7 i) (/ 1.0 16)))
         elseif (<= 8 i 15) 
           then (setf (svref res i) (* (- i 7) (/ 1.0 8)))
         elseif (< 15 i 24)
           then (setf (svref res i) (- 1.0 (* (- i 15) (/ 1.0 16))))))
      res))
                
                      
                
                
      


(defun build-station (name min max pct-rain lat long blank-nodes-p attributes-p)
  ;; min/max are temperatures in Fahrenheit
  ;; pct rain is a number from 0  to 1.0

  (let ((station (resource (format nil "~ast_~a" *wx-namespace* name)))
        (attributes (if* attributes-p
                       then `(("name" . ,name)))))
                            
    (add-triple station !rdf:type !wx:station :g station :attributes attributes)
    (add-triple station !rdf:label (literal name) :g station :attributes attributes)
    (add-triple station !wx:location
                (geo-plist->geo *lat-long-nd-subtype* :lat lat :lon long)
                :g station
                :attributes attributes)
           
    (do ((time *time-min* (+ time *sample-interval*))
         
         ;; in-day is the seconds within a day
         (in-day 0  (mod (+ in-day *sample-interval*)
                         #.(* 24 60 60))))
        ((> time *time-max*))
      (let ((bn (get-new-blank-node blank-nodes-p)))
        (add-triple bn !rdf:type !wx:observation :g station :attributes attributes)
        (add-triple bn !wx:time  (value->upi (cons time 0) :date-time) :g station
                    :attributes attributes)
        (add-triple bn !wx:temp  (value->upi (compute-temp min max in-day)
                                             :integer)
                    :g station
                    :attributes attributes
                    )
        
        (if* (<= (random 1.0  *wx-random-state*) pct-rain)
           then ;; it rained
                (add-triple bn !wx:wx !wx:rain :g station :attributes attributes)
                ;; rain amount from 0 to 10 mm
                (add-triple bn !wx:precip (value->upi (truncate (random 10 *wx-random-state*)) :integer) :g station
                            :attributes attributes)
           else (add-triple bn !wx:wx !wx:clear :g station :attributes attributes))
        ))))

(defun get-new-blank-node (real-bn)
  (if* real-bn
     then (new-blank-node)
     else (resource (format nil "xb~d" (incf *blank-node-counter*)) "wx")))
           


(defun compute-temp (min max secs)
  ;;
  ;; secs is number of seconds since midnight
  ;;
  (let ((hour (truncate secs (* 60 60))))
    ;; hour will be between 0 and 23
    (truncate (+ min (* (svref *temp-factors* hour) (- max min))))))

    
               
(defun compute-station-name (i)
  (format nil "~a~a~a"
          (code-char (+ (char-code #\a) (mod (truncate i (* 26 26)) 26)))
          (code-char (+ (char-code #\a) (mod (truncate i 26) 26)))
          (code-char (+ (char-code #\a) (mod i 26)))))
                     
          
          
                
(defun build-db (&key (dbname *dbname*)  (stations 100) (blank-nodes-p t)
                      (attributes-p nil))
  (create-triple-store dbname)
  (unwind-protect
      (progn
        (setf (automate-nd-datatype-mappings :db *db*) t)
        (setq *lat-long-nd-subtype*
          (geo-register-nd-subtype :ordinates *lat-long-nd-ordinates*
                                   :install t))
        (add-geospatial-subtype-to-db *lat-long-nd-subtype* *db*)
        
        
        (if* attributes-p
           then ;; store station name as an attribute as well
                ;; just to test out store/export of attributes
                (define-attribute "name"))
        
        (setup-mappings)
        (commit-triple-store)
        
        (let ((*wx-random-state* (make-random-state *wx-random-seed*))
              (*blank-node-counter* 0))

          
          (with-buffered-triple-adds (*db*)
            (dotimes (i stations)
              ;; choose random min and max temps
              (let* ((min (- 50 (random 20 *wx-random-state*)))
                     (max (+ 70 (random 10  *wx-random-state*))))
                (build-station (compute-station-name i)
                               min
                               max 
                               *pct-rain*
                               (random *lat-max* *wx-random-state*)
                               (random *long-max* *wx-random-state*)
                               blank-nodes-p
                               attributes-p
                               )
      
                (if* (zerop (mod (1+ i) 100)) 
                   then (commit-triple-store)
                        (format t "~a: ~d stations~%" (excl.osi:ctime) 
                                (1+ i)))
                ))
            (commit-triple-store)))
    
        ;; cleanup
        (close-triple-store))))

(defun setup-mappings ()
  (register-namespace "wx" *wx-namespace*)
  (setf (db.agraph::predicate-mapping !wx:location) *lat-long-nd-subtype*)
  )
  
             

      


;;;; Queries


;; 
;;
;;
;; To run a query, using all defaults just
;;  (setup-for-query)
;;  (runq *q1*)
;;  (runq *q2*)
;;  ...
;;
;;
;; setup-for-query takes a :dbname argument and simply opens the database.
;;
;; By default runq  will return the count of results.  This is useful for measuring
;; the speed of the sparql engine to compute the results independent of the time to
;; transmit them back to the caller.
;;
;; To  see the actual results you can supply a value for the :format argument
;; e.g.
;;  (runq *q1* :format :lists)
;;
;;


(defun setup-for-query (&key (dbname *dbname*))
  (open-triple-store dbname))

(defun runq (query &key (format :count))
  (time (sparql:run-sparql query :results-format format)))




;; q1 - show the top 15 stations with the highest average temps

(defparameter *q1* "
prefix wx: <http://demo.com/wx#>
prefix ndfn: <http://franz.com/ns/allegrograph/5.0/geo/nd/fn#> 
prefix : <http://franz.com/ns/keyword#>

select ?station (avg(?temp) as ?avg) (min(?temp) as ?min) (max(?temp) as ?max) ?lat ?lon {

   ?station rdf:type wx:station .
   ?station wx:location ?loc .
   bind(ndfn:ordinateValue(?loc,:lat)  as ?lat) .
   bind(ndfn:ordinateValue(?loc,:lon)  as ?lon) . 
   graph ?station {
             ?obs    wx:temp ?temp .
   }
}  group by ?station ?lat ?lon
order by desc(?avg)
limit 15


")



;; q2 - show the stations in a 200km radius of 5 degrees latitude
;; and 6 degrees longitude

(defparameter *q2* "
prefix wx: <http://demo.com/wx#>
prefix nd: <http://franz.com/ns/allegrograph/5.0/geo/nd#>
prefix : <http://franz.com/ns/keyword#>
select ?station ?lat ?lon {

   ?station nd:inCircle (
                 wx:location
                 :lat 5.0 :lon 6.0 
                 :units :km :radius 200.0) .
   ?station wx:location ?loc .
   bind(ndfn:ordinateValue(?loc,:lat)  as ?lat) .
   bind(ndfn:ordinateValue(?loc,:lon)  as ?lon) . 


}
")


;; show 15 top stations by rainfall, and their
;; average temps

(defparameter *q3* "
prefix wx: <http://demo.com/wx#>

select ?station (sum(?rainfall) as ?rain) (avg(?temp) as ?avgtemp){

   graph ?station {
       ?obs wx:precip ?rainfall .
       ?obs wx:temp   ?temp
   }

} group by ?station
  order by desc(?rain)
  limit 15

")



		       
    
              
      
      
      
    
        
  
  
