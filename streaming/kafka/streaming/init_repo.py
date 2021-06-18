import bz2
import logging
import os
import sys

import importlib_resources
from franz.openrdf.connect import ag_connect
from franz.openrdf.rio.rdfformat import RDFFormat

from . import data as module_data

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
ch = logging.StreamHandler()
formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
ch.setLevel(logging.INFO)
ch.setFormatter(formatter)
logger.addHandler(ch)

SNA_RQ = """PREFIX    : <http://franz.com/examples/>
PREFIX sna: <http://franz.com/ns/allegrograph/4.11/sna/>

DELETE { graph sna:sna { ?id ?p ?o }}
WHERE {
  GRAPH sna:sna {
    ?id a sna:Generator ;
        sna:hasName :connectedPosts ;
        ?p ?o  .
  }
};

INSERT data {
  GRAPH sna:sna {
   [ a sna:Generator ;
       sna:hasName :connectedPosts ;
       sna:undirected ( :retweetOf :replyTo ) ;
   ]
  }
}"""

AG_ENV = {
    "host": os.environ.get("AGRAPH_HOST", "localhost"),
    "port": os.environ.get("AGRAPH_PORT", "10035"),
    "user": os.environ.get("AGRAPH_USER", "test"),
    "password": os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
}


def main():
    repo = os.environ.get("AGRAPH_REPO", "stremaing-demo")
    with ag_connect(
        repo,
        **AG_ENV,
        create=True,
        clear=True,
    ) as conn:
        logger.info("AllegroGraph repo '%s' connected" % repo)
        with importlib_resources.path(module_data, "data.nt.bz2").open("rb") as fd:
            data = bz2.decompress(fd.read()).decode("utf-8")
            conn.addData(data, rdf_format=RDFFormat.NTRIPLES)
        logger.info("Triples added to the repo")

        flag = conn.prepareUpdate(query=SNA_RQ).evaluate()
        if flag:
            logger.info("SNA clauses successfully added")
        else:
            logger.error("Failed to insert SNA clauses\nAbort!")
            sys.exit(1)

        conn.deleteDuplicates("spo")
        conn.commit()
        logger.info(
            "Repo is ready at http://%s:%s/#/repositories/%s"
            % (
                AG_ENV["host"],
                AG_ENV["port"],
                repo,
            )
        )


if __name__ == "__main__":
    main()
