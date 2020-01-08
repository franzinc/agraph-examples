# Weather data generator

This code here generates weather observation data for a given number of weather stations over a certain time period.
This data can be written to a distributed repository that is
partitioned on the graph part of the quad.

See the file wxdemo.cl for details on how to build the weather observation database.


## quads generated

The quads generated, in a pseudo-ntriples syntax, are shown here.
For each station there are quads that provide the station name
and location.
For each observation at the station there are quads that provide
the time of the observation and the weather observed.  If it was
raining then the rainfall accumulation since the last observation
is provided.

wx here is "http://demo.com/wx#"

```
wx:st_<stationname>  rdf:type      wx:station       wx:st_<stationname>
wx:st_<stationname>  rdf:label    "stationname"     wx:st_<stationname>
wx:st_<stationname>  wx:location   lat-long         wx:st_<stationname>
_bn2                 rdf:type      wx:observation   wx:st_<stationname>
_bn2                 wx:time       "2018-05-07T18:00:00Z"^^xsd:dateTime	  wx:st_<stationname>
_bn2                 wx:temp       "20"^^xsd:integer wx:st_<stationname>
_bn2                 wx:wx         wx:rain          wx:st_<stationname>      [[if rain]]
_bn2                 wx:precip     "5"              wx_st:_<stationname>      [[if rain]]
_bn2                 wx:wx         ws:clear         wx:st_<stationname>      [[if no rain]]
```
