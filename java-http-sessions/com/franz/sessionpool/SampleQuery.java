/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.franz.sessionpool;

/**
 *
 * @author jkf
 */
public class SampleQuery {
    public static final String sampleQueryString = 
  String.join( ""
 ,"PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
 ,"PREFIX dc:      <http://purl.org/dc/elements/1.1/>\n"
 ,"PREFIX dcterms: <http://purl.org/dc/terms/>\n"
 ,"PREFIX bench:   <http://localhost/vocabulary/bench/>\n"
 ,"PREFIX xsd:     <http://www.w3.org/2001/XMLSchema#> \n"
 ,"\n"
 ,"SELECT ?yr\n"
 ,"WHERE {\n"
 ,"  ?journal rdf:type bench:Journal .\n"
 ,"  ?journal dc:title \"Journal 1 (1940)\"^^xsd:string .\n"
 ,"  ?journal dcterms:issued ?yr \n"
 ,"}\n"
 ,"\n");
}
