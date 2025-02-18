import requests
from franz.openrdf.connect import ag_connect
from typing import Optional

def create_nlq_vdb(
    repo_name,
    conn,
    vdb_name,
    openai_api_key: str = '',
    host: str = 'localhost',
    port: str = '10035',
    user: str = 'test',
    password: str = 'xyzzy',
    protocol: str = 'http',
    embedder: str = 'openai',
    embedding_model: str = 'text-embedding-3-small'
    ):
    
    #add shacl to connection
    url = (
        f"{protocol}://{user}:{password}@{host}:{port}/repositories/{repo_name}/data-generator/shacl"
    )
    response = requests.get(url)

    # add shacl data to repo
    conn.addData(response.json())
    
    # create vdb
    nlq_conn = ag_connect(
        vdb_name,
        clear=True,
        create=True,
        host=host,
        port=port,
        user=user,
        password=password,
    )
    
    #convert to vector store
    nlq_conn.convert_to_vector_store(
        embedder,
        api_key=openai_api_key,
        model=embedding_model
    )
   
    # create the connection between regular connection and vector store
    # create a linking repo (just connect if it already exists)
    connect_conn = ag_connect(
        "nlq-to-store-relationship",
        user=user,
        password=password,
        host=host,
        port=port
    )

    # add linking data
    connect_conn.executeUpdate(
        f"""
        prefix gen: <http://franz.com/vdb/gen/>
        insert data {{
            <http://franz.com/test_example> gen:catalog "/" ;
                gen:repository "{repo_name}" ;
                gen:nlq-vdb "{vdb_name}" . }}
        """
    )
    connect_conn.deleteDuplicates(mode="spo")
    connect_conn.close() 
    
    return nlq_conn
    