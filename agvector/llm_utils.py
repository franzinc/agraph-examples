from franz.openrdf.connect import ag_connect
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.model.value import URI
from franz.openrdf.vocabulary import RDF, RDFS
import urllib.request
from pprint import pprint
import datetime
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.schema.document import Document
import shortuuid
import textwrap

def read_text(url: str):
    with urllib.request.urlopen(url) as f:
        contents = f.read().decode('utf-8')
    return contents

def print_text(string: str):
    string = string.replace('\\n', '').replace('\\r', ' ')
    wrapper = textwrap.TextWrapper(width=100)
    word_list = wrapper.wrap(text=string)
    for element in word_list:
        print(element)
        
class FindNearestNeighbors:
    def __init__(self, conn, phrase, vector_db, number=10, confidence=.5):
        self.conn = conn
        self.f = conn.namespace('http://franz.com/llm/')
        self.phrase = phrase
        self.vector_db = vector_db
        self.number = number
        self.confidence = confidence
        self.df = self.query(conn, phrase, vector_db, number, confidence)
        try:
            print_text(self.df['originalText'][0])
        except:
            print(f"No Neighbor found with confidence score {confidence}")
    
    def query(self, conn, phrase, vector_db, number, confidence):
        query_string = f"""
            PREFIX llm: <http://franz.com/ns/allegrograph/8.0.0/llm/> 
            select * where {{
                (?uri ?score ?originalText) llm:nearestNeighbor ("{phrase}" "{vector_db}" {str(number)} {str(confidence)})  }}"""
        with conn.executeTupleQuery(query_string) as result:
            df = result.toPandas()
        return df
        
    def proof(self):
        for i in range(self.df.shape[0]):
            print(i, self.df['uri'][i], self.df['score'][i])
            print_text(self.df['originalText'][i])
            print()

    def add_neighbors_to_graph(self):
        neighbor_uri = self.conn.createURI(f'http://franz.com/llm/neighbor/{shortuuid.uuid()}')
        triples = [
            (neighbor_uri, RDF.TYPE, self.f.Neighbor, neighbor_uri),
            (neighbor_uri, self.f.phrase, self.conn.createLiteral(self.phrase), neighbor_uri),
            (neighbor_uri, self.f.vectorDB, self.vector_db, neighbor_uri),
            (neighbor_uri, self.f.confidence, self.confidence, neighbor_uri),
            (neighbor_uri, self.f.datetime, datetime.datetime.now(), neighbor_uri)]
        for i in range(self.df.shape[0]):
            neighbor_score = self.conn.createBNode()
            triples.append((neighbor_uri, self.f.link, neighbor_score, neighbor_uri))
            triples.append((neighbor_score, self.f.confidenceScore, self.df['score'][i], neighbor_uri))
            triples.append((neighbor_score, self.f.index, i, neighbor_uri))
            triples.append((neighbor_score, self.f.neighbor, self.conn.createURI(self.df['uri'][i][1:-1]), neighbor_uri))
        self.conn.addTriples(triples)
        
def clear_neighbors(conn):
    query_string = """select ?neighbor where { ?neighbor a <http://franz.com/llm/Neighbor> }"""
    with conn.executeTupleQuery(query_string) as result:
        df = result.toPandas()
    for neighbor in list(df['neighbor']):
        conn.remove(None, None, None, neighbor)

class AskMyDocuments:
    def __init__(self, conn, question, vector_db, number=10, confidence=.5):
        self.conn = conn
        self.f = conn.namespace('http://franz.com/llm/')
        self.question = question
        self.vector_db = vector_db
        self.number = number
        self.confidence = confidence
        self.df = self.query(conn, question, vector_db, number, confidence)
        try:
            print_text(self.df['response'][0])
        except:
            print(f"No response found with confidence score {confidence}")
    
    def query(self, conn, question, vector_db, number, confidence):
        query_string = f"""PREFIX llm: <http://franz.com/ns/allegrograph/8.0.0/llm/>
                        select * where {{
                            (?response ?score ?citation ?content) llm:askMyDocuments ("{question}" "{vector_db}"  {str(number)} {str(confidence)}). }}"""
        with conn.executeTupleQuery(query_string) as result:
            df = result.toPandas()
        return df
    
    def proof(self):
        for i in range(self.df.shape[0]):
            print(i, self.df['score'][i], self.df['citation'][i])
            print_text(self.df['content'][i])
            print()

    def add_evidence_to_graph(self):
        if self.df.shape[0] > 0:
            evidence_uri = self.conn.createURI(f'http://franz.com/llm/evidence/{shortuuid.uuid()}')
            triples = [
                (evidence_uri, RDF.TYPE, self.f.Question, evidence_uri),
                (evidence_uri, self.f.question, self.conn.createLiteral(self.question), evidence_uri),
                (evidence_uri, self.f.vectorDB, self.vector_db, evidence_uri),
                (evidence_uri, self.f.confidence, self.confidence, evidence_uri),
                (evidence_uri, self.f.datetime, datetime.datetime.now(), evidence_uri),
                (evidence_uri, self.f.response, self.conn.createLiteral(self.df['response'][0]), evidence_uri)]
            for i in range(self.df.shape[0]):
                evidence_score = self.conn.createBNode()
                triples.append((evidence_uri, self.f.link, evidence_score, evidence_uri))
                triples.append((evidence_score, self.f.confidenceScore, self.df['score'][i], evidence_uri))
                triples.append((evidence_score, self.f.index, i, evidence_uri))
                triples.append((evidence_score, self.f.evidence, self.conn.createURI(self.df['citation'][i][1:-1]), evidence_uri))
            self.conn.addTriples(triples) 
        else:
            print("No evidence found")

def clear_questions(conn):
    query_string = """select ?question where { ?question a <http://franz.com/llm/Question> }"""
    with conn.executeTupleQuery(query_string) as result:
        df = result.toPandas()
    for question in list(df['question']):
        conn.remove(None, None, None, question)

class BufferTriples:
    def __init__(self, conn, max_size=10000):
        self.conn = conn
        self.buffer_triples = []
        self.max_size = max_size
    def add(self, triple):
        if len(self.buffer_triples) < self.max_size:
            self.buffer_triples.append(triple)
        else:
            self.conn.addTriples(self.buffer_triples)
            self.buffer_triples = [triple]
    def flush_triples(self):
        self.conn.addTriples(self.buffer_triples)
        self.buffer_triples=[]
        
def addArbitraryTextString(conn, f, buffer, text, id, chunk_size=1000, chunk_overlap=10):
    documents = [Document(page_content=text)]
    text_splitter = RecursiveCharacterTextSplitter(chunk_size=chunk_size, chunk_overlap=chunk_overlap)
    docs = text_splitter.split_documents(documents)
    
    if isinstance(id, str): id_uri = conn.createURI(id)
    elif isinstance(id, URI): id_uri = id
    elif isinstance(id, int): id_uri = conn.createURI(f"http://franz.com/llm/{str(id)}")
    id_str = id_uri.localname
    
    for i, doc in enumerate(docs):
        doc_id = conn.createURI(f"http://franz.com/llm/{id_str}_{str(i)}")
        buffer.add((id_uri, f.chunk, doc_id, id_uri))
        buffer.add((doc_id, RDF.TYPE, f.Chunk, id_uri))
        content = docs[i].page_content
        buffer.add((doc_id, f.text, docs[i].page_content, id_uri))
        buffer.add((doc_id, f.section, i, id_uri))        
    return buffer


