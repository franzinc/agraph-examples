;; Last edited by JKF and JA on 06/08/23

;;; Instructions to run this demo:
;;;
;;; edit the defparameter settings below based on where agraph is installed
;;; One of the parameters is the google api key
;;;   you get a google api key this way: <explain>
;;; 
;;; compile and load this file
;;; eval: (start-demo)
;;;
;;; go to the url printed to see the demo. 
;;; click to change the location to search.
;;;

(in-package :user)


(eval-when (compile load eval)

  
  ;; ;; ;; set these values based on your agraph installation
  ;; ;; ;; and your google api key
  
  (defparameter *agraph-fasl-location* "~/ag731smp/lib/agraph.fasl")
  (defparameter *agraph-server-port* 10035)
  (defparameter *webserver-port* 10027) 
  (defparameter *api-key* "your-api-key-here") 

  ;; ;; ;;
  

  (require :agraph *agraph-fasl-location*)
  (require :net-xml-generator)
  (use-package :net.xml.generator)
  (require :aserve)
  (use-package :net.aserve.client)
  (require :aserve)
  (require :datetime)
  
) ; end eval-when

(defpackage :user (:use :db.agraph))

(eval-when (compile load eval)
  (enable-!-reader)
  (set-xml-generator-macro-chars #\^ #\@))








(defun start-demo ()
  (or (triple-store-exists-p "PhoneCalls" :port *agraph-server-port*)
      (let (*db*)
        (create-triple-store "PhoneCalls" :port *agraph-server-port*)
        (setf (automate-nd-datatype-mappings) t)
        (load-turtle "./PhoneCalls.ttl") 
        (commit-triple-store)))
  (create-agpool "PhoneCalls")
  (start-map-server :port *webserver-port*)	
  
  (format t "~2%in a web browser visit this url: http://localhost:~d~2%" *webserver-port*)
  )
  


j






(defstruct agpool
  list
  name)

(defparameter .agpool. nil)

(defun create-agpool (name &optional (max 2))
  (let (*db*)
    (setf .agpool. (make-agpool :name name :list nil))
    (dotimes (i max)
      (push (open-triple-store name :port *agraph-server-port*) (agpool-list .agpool.)))))

(defun agpoolpop ()
  (let ((db (pop-atomic (agpool-list .agpool.))))
    (or db
        (let (*db*) (open-triple-store (agpool-name .agpool.) :port *agraph-server-port*)))))

(defun agpoolpush (db)
  (push-atomic db (agpool-list .agpool.)))





(defparameter *month-alist*
    '(("Jan" .  "1")
      ("Feb" .  "2")
      ("Mar" .  "3")
      ("Apr" .  "4")
      ("May" .  "5")
      ("Jun" .  "6")
      ("Jul" .  "7")
      ("Aug" .  "8")
      ("Sep" .  "9")
      ("Oct" . "10")
      ("Nov" . "11")
      ("Dec" . "12")))

(defparameter *initial-alist*
    (copy-tree
     '(("latitude" . "+37.780") ("longitude" . "-122.415") ("radius" . "0.20") ("delta" . "300")
       ("year" . "2014") ("month" . "Aug") ("day" . "18") ("hour" . "13") ("minute" . "23") ("second" . "00")
       ("center-lat" . "+37.780") ("center-lon" . "-122.415") ("zoom" . "15")
       ("initial-display" . "1")
       )))

#+never
(defparameter *japantown*
    '(
      (37.78462194814176d0 . 122.43122220039368d0)
      (37.78491024065857d0 . 122.42802500724792d0)
      (37.78675019875761d0 . 122.42835760116577d0)
      (37.78809834174111d0 . 122.42864727973938d0)
      (37.78799659595888d0 . 122.43026733398438d0)
      (37.787479386066565d0 . 122.43022441864014d0)
      (37.78702152510845d0 . 122.43353962898254d0)
      ))

;; This runs in the direct lisp client and isn't the same as the ag server.
(defun start-map-server (&key (port 10027))
  (register-namespace "phone" "http://phonecalls.com/phone-calls#")
  (net.aserve:start :port port :external-format :utf-8)
  (net.aserve:publish :path "/"             :function 'main-page)
  (net.aserve:publish :path "/index.html"   :function 'main-page)
  (net.aserve:publish :path "/update/"      :function 'main-page)
  t)

(defmacro with-http-response-body ((stream-var req ent &rest rest) &body body)
  `(net.aserve:with-http-body (,req ,ent ,@rest)
     (let ((,stream-var net.html.generator:*html-stream*)
	   (*print-level* nil) (*print-length* nil))
       ,@body)))

(defun main-page (req ent)
  (net.aserve:with-http-response (req ent)
    (with-http-response-body (stm req ent :external-format :utf-8)
      (let ((query-alist (net.aserve:request-query req))
	    (*db* (agpoolpop)))
	  (flet ((v (name)
		   (cdr (or (name->value name query-alist)
			    (name->value name *initial-alist*)
			    (error "Missing query parameter value: ~s" name)))))
	    ;; (format *terminal-io* "Before ut~%")
	    (let* ((ut (encode-universal-time
			(read-from-string (v "second"))
			(read-from-string (v "minute"))
			(read-from-string (v "hour"))
			(read-from-string (v "day"))
			(read-from-string (cdr (name->value (v "month") *month-alist*)))
			(read-from-string (v "year"))
			+7))
		   (lat    (v "latitude"))
		   (lon    (v "longitude"))
		   (radius (v "radius"))
		   (delta  (v "delta"))
		   (points #-notyet (search-for-calls :lat (read-from-string lat)
						      :lon (read-from-string lon)
						      :radius (read-from-string radius)
						      :time ut
						      :delta (read-from-string delta))
			   #+notyet *test-points*))
	      lat lon radius delta
	      (rollback-triple-store)	; In case some other client has been changing the db.
	      (agpoolpush *db*)
	      (control-publisher stm query-alist ut points)))))))

(defun control-publisher (stm query-alist ut points)
  (declare (ignorable ut))
  ;;(format t "~s~%" (stream-external-format stm))
  #-bug (format stm "<!DOCTYPE html>~%")
  #+bugx (format stm "<!DOCTYPE html encoding=\"UTF-8\" >~%")
  (with-xml-generation (stm)
    #+bug (write-doctype stm "html" "")
    (flet ((v (name)
	     (cdr (or (name->value name query-alist)
		      (name->value name *initial-alist*)
		      (error "Missing query parameter value: ~s" name)))))
      ^(html
	^(head
	  ^((meta @http-equiv "Content-Type" @content "text/html; charset=utf-8"))
	  ^(title "Proximate Phone Calls")
	  (emit-map-header-stuff points
				 (read-from-string (v "latitude"))
				 (read-from-string (v "longitude"))
				 (* 1000 (read-from-string (v "radius")))
				 (read-from-string (v "zoom"))
				 (read-from-string (v "center-lat"))
				 (read-from-string (v "center-lon"))))
	^(body
	  #+never ^(p @(prin1-to-string query-alist) ^br @(excl.osi:ctime ut))
	  ^((form @id "mapId" @action "/update/" @method "post")
	    ^(p
	      ^((table @width "90%" @align "center")
		^(tr
		  (let ((count (ash (length points) -1)))
		    ^(td ^(big ^(b @count) @(format nil " phone call~p in this region" count))))
		  ^(td ^(small "This sample db has only two hours of data starting at 2014-08-18T12:00:00-08"))
		  ^(td ^((input @type "SUBMIT" @value "Query")))
		  ^(td ^((input @type "RESET")))
		  ^(td " ")))
	      ^((input @type "HIDDEN" @id "zoom" @name "zoom" @value (v "zoom")))
	      ^((input @type "HIDDEN" @id "center-lat" @name "center-lat" @value (v "center-lat")))
	      ^((input @type "HIDDEN" @id "center-lon" @name "center-lon" @value (v "center-lon")))
	      ^((input @type "HIDDEN" @id "center-lon" @name "initial-display" @value "0"))
	      ^((table @align "center")
		^(thead ^(tr))
		^(tr ^(th "latitude" ) ^(th "longitude") ^(th "radius in km")
		     ^(th "year") ^(th "mon") ^(th "day") ^(th "hour") ^(th "min") ^(th "sec")
		     ^(th " ") ^(th "±seconds"))
		^(tr ^(td ^((input @type "text" @name "latitude"  @id "latitude"  @value (v "latitude") @size 11)))
		     ^(td ^((input @type "text" @name "longitude" @id "longitude" @value (v "longitude") @size 11)))
		     ^(td ^((input @type "text" @name "radius"    @value (v "radius") @size 7)))
		     ^(td ^((select @name "year")
			    ^((option @label "2014" @value "2014") "2014")))
		     (let ((v (v "month")))
		       ^(td ^((select @name "month")
			      (loop for (name . nil) in *month-alist*
				  do ^((option @label name @value name
					       (and (equal name v) @selected "t"))
				       @name)))))
		     (let ((v (v "day")))
		       ^(td ^((select @name "day")
			      (loop for i from 1 to 31 ; short months could throw error
				  as istr = (princ-to-string i)
				  do ^((option @label istr @value istr
					       (and (equal istr v) @selected "t"))
				       istr)))))
		     (let ((v (v "hour")))
		       ^(td^((select @name "hour")
			     (loop for i from 0 to 23
				 as istr = (princ-to-string i)
				 do ^((option @label istr @value istr
					      (and (equal istr v) @selected "t"))
				      istr)))))
		     (let ((v (v "minute")))
		       ^(td ^((select @name "minute"))
			    (loop for i from 0 to 59
				as istr = (princ-to-string i)
				do ^((option @label istr @value istr
					     (and (equal istr v) @selected "t"))
				     istr))))
		     (let ((v (v "second")))
		       ^(td ^((select @name "second"))
			    (loop for i from 0 to 59
				as istr = (princ-to-string i)
				do ^((option @label istr @value istr
					     (and (equal istr v) @selected "t"))
				     istr))))
		     ^(td "-07:00")
		     ^(td ^((input @name "delta" @type "text" @value (v "delta") @size 7 @align "right")))))))
	  ^((div @id "map-canvas"))
	  )))))


(defun name->value (name  alist) (assoc  name  alist :test 'equal))
(defun value->name (value alist) (rassoc value alist :test 'equal))

(defvar *last-points* nil)

(defun emit-map-header-stuff (points lat lon rad zoom center-lat center-lon)
  (setf *last-points* points)
  ^((meta @name "viewport" @content "initial-scale=1.0, user-scalable=no"))
  ^((style @type "text/css")
    "
 html { height: 100% }
 body { height: 100%; margin: 0; padding: 0 }
 #map-canvas { height: 88% } ")
  ^((script @type "text/javascript" @src (format nil "https://maps.googleapis.com/maps/api/js?key=~a&callback=initializeMap&v=weekly" *api-key*)) "") ; May not be empty?
  ^((script @type "text/javascript")
    @(format nil "
function mouseClick(mouse_event) {
  var lat_lon = mouse_event.latLng;
  var newLat = document.getElementById(\"latitude\");
  var newLon = document.getElementById(\"longitude\");
  newLat.value = lat_lon.lat();
  newLon.value = lat_lon.lng();
  document.getElementById('mapId').submit();
  }

function initialize() {
   var mapOptions = {
	      center: new google.maps.LatLng(~g, ~g),
	      zoom: ~d,
              scaleControl: true
	      };
   var map = new google.maps.Map(document.getElementById(\"map-canvas\"), mapOptions);
   map.zoom_changed = function() {
     var zoomInput = document.getElementById(\"zoom\");
     zoomInput.value = map.getZoom();
     }
   map.center_changed = function() {
     var newCenterLat = document.getElementById(\"center-lat\");
     var newCenterLon = document.getElementById(\"center-lon\");
     var lat_lon = map.getCenter();
     newCenterLat.value = lat_lon.lat();
     newCenterLon.value = lat_lon.lng();
     }
   google.maps.event.addListener(map, 'click', mouseClick);
"
	     center-lat center-lon zoom)
    ;; https://developers.google.com/maps/documentation/javascript/examples/event-arguments

    @(format nil "
  var circleOptions = {
    strokeColor: '#FF0000',
    strokeOpacity: 0.5,
    strokeWeight: 2,
    fillColor: '#800000',
    fillOpacity: 0.10,
    map: map,
    center: new google.maps.LatLng(~g, ~g),
    radius: ~g,
    clickable: 'false'
    };
  var circle = new google.maps.Circle(circleOptions);
  google.maps.event.addListener(circle, 'click', mouseClick);
"
	     lat lon rad)
    (loop for ((number1 latitude1 longitude1 caller1 time) (number2 latitude2 longitude2 caller2)) on points by #'cddr
	do ;; ~g isn't optimal but none of our ordinates are near the equator or prime meridian
	  @(format nil "
  var marker = new google.maps.Marker({
     icon: '~a',
     position: new google.maps.LatLng(~g,~g),
     map: map,
     title: \"~a\"
  });
  google.maps.event.addListener(marker, 'click', mouseClick);
"
		   ;;"https://maps.gstatic.com/mapfiles/ms2/micons/phone.png"
		   (if caller1
		       "http://maps.google.com/mapfiles/ms/icons/red-dot.png"
		     "http://maps.google.com/mapfiles/ms/icons/blue-dot.png")
		   latitude1 longitude1 (format nil "~a\\n~a" number1
						(locale-print-time (util.date-time:ut-to-date-time time)
								   :fmt "%H:%M:%S" :stream nil)))
	  @(format nil "
  var marker = new google.maps.Marker({
     icon: '~a',
     position: new google.maps.LatLng(~g,~g),
     map: map,
     title: \"~a\"
  });
  google.maps.event.addListener(marker, 'click', mouseClick);
"
		   ;;"https://maps.gstatic.com/mapfiles/ms2/micons/phone.png"
		   (if caller2
		       "http://maps.google.com/mapfiles/ms/icons/red-dot.png"
		     "http://maps.google.com/mapfiles/ms/icons/blue-dot.png")
		   latitude2 longitude2 (format nil "~a\\n~a" number2
						(locale-print-time (util.date-time:ut-to-date-time time)
								   :fmt "%H:%M:%S" :stream nil)))
	  @(format nil "
  var connect = new google.maps.Polyline({
    path: [ new google.maps.LatLng(~g, ~g), new google.maps.LatLng(~g, ~g) ],
    geodesic: true,
    strokeColor: '#8020C0',
    strokeOpacity: 0.5,
    strokeWeight: 2,
    map: map
  });
"
		   latitude1 longitude1 latitude2 longitude2))
    "
 }
 google.maps.event.addDomListener(window, 'load', initialize);
 ")
  )

#|

A custom placemerk edits on GE:

<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
<Document>
	<name>KmlFile</name>
	<Style id="s_ylw-pushpin_hl">
		<IconStyle>
			<color>f2ff5500</color>
			<scale>1.18182</scale>
			<Icon>
				<href>http://maps.google.com/mapfiles/kml/shapes/phone.png</href>
			</Icon>
			<hotSpot x="0.5" y="0" xunits="fraction" yunits="fraction"/>
		</IconStyle>
		<LabelStyle>
			<color>ff00aa55</color>
		</LabelStyle>
		<ListStyle>
		</ListStyle>
	</Style>
	<Style id="s_ylw-pushpin">
		<IconStyle>
			<color>f2ff5500</color>
			<Icon>
				<href>http://maps.google.com/mapfiles/kml/shapes/phone.png</href>
			</Icon>
			<hotSpot x="0.5" y="0" xunits="fraction" yunits="fraction"/>
		</IconStyle>
		<LabelStyle>
			<color>ff00aa55</color>
		</LabelStyle>
		<ListStyle>
		</ListStyle>
	</Style>
	<StyleMap id="m_ylw-pushpin">
		<Pair>
			<key>normal</key>
			<styleUrl>#s_ylw-pushpin</styleUrl>
		</Pair>
		<Pair>
			<key>highlight</key>
			<styleUrl>#s_ylw-pushpin_hl</styleUrl>
		</Pair>
	</StyleMap>
	<Placemark>
		<LookAt>
			<longitude>-122.2944119991923</longitude>
			<latitude>37.87026931594495</latitude>
			<altitude>0</altitude>
			<heading>1.133505160708057e-009</heading>
			<tilt>44.99945433702622</tilt>
			<range>997.0657418225432</range>
			<gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>
		</LookAt>
		<styleUrl>#m_ylw-pushpin</styleUrl>
		<Point>
			<gx:drawOrder>1</gx:drawOrder>
			<coordinates>-122.2976204826626,37.87114883152403,0</coordinates>
		</Point>
	</Placemark>
</Document>
</kml>

|#

#|

var flightPlanCoordinates = [
    new google.maps.LatLng(37.772323, -122.214897),
    new google.maps.LatLng(21.291982, -157.821856),
    new google.maps.LatLng(-18.142599, 178.431),
    new google.maps.LatLng(-27.46758, 153.027892)
  ];
  var flightPath = new google.maps.Polyline({
    path: flightPlanCoordinates,
    geodesic: true,
    strokeColor: '#FF0000',
    strokeOpacity: 1.0,
    strokeWeight: 2
  });

|#

(defun search-for-calls (&key lat lon radius time delta)
  (loop for (p1 p2 p3 p4 callerp)
      in '((!phone:originLocation
	    !phone:targetLocation
	    !phone:Originator
	    !phone:Receiver
	    t)
	   (!phone:targetLocation
	    !phone:originLocation
	    !phone:Receiver
	    !phone:Originator
	    nil))
      nconc
	(loop				; repeat 100
	    with c = (get-triples-nd-within-radius :p p1 :debug nil
						   :latitude lat :longitude lon
						   :radius radius :units :km
						   :time-min (- time delta)
						   :time-max (+ time delta))
	    as tr = (cursor-next-row c)
	    while tr
	    nconc (let* ((call   (subject tr))
			 (caller (part->terse (object (get-triple :s call :p p3))))
			 (callee (part->terse (object (get-triple :s call :p p4))))
			 (caller-where (geo-upi->plist (object tr)))
			 (callee-where (geo-upi->plist (object (get-triple :s call :p p2)))))
		    (destructuring-bind (&key ((:latitude caller-latitude))
					      ((:longitude caller-longitude))
					      ((:time time1))
					 &allow-other-keys)
			caller-where
		      (destructuring-bind (&key ((:latitude callee-latitude))
						((:longitude callee-longitude))
						((:time time2)))
			  callee-where
			(list (list caller caller-latitude caller-longitude callerp time1)
			      (list callee callee-latitude callee-longitude (not callerp) time2))))))))

(defun compute-data-limits ()
  (loop with cursor = (get-triples :p !phone:originLocation)
      as triple = (cursor-next-row cursor)
      while triple
      as plist = (geo-upi->plist (object triple))
      as tim = (getf plist :time)
      as lat = (getf plist :latitude)
      as lon = (getf plist :longitude)
      count 1 into c
      minimize tim into tim-min
      maximize tim into tim-max
      minimize lat into lat-min
      maximize lat into lat-max
      minimize lon into lon-min
      maximize lon into lon-max
      finally (return (values c
			      (princ-to-string (util.date-time:ut-to-date-time (truncate tim-min)))
			      (princ-to-string (util.date-time:ut-to-date-time (ceiling tim-max)))
			      lat-min lat-max lon-min lon-max))))
