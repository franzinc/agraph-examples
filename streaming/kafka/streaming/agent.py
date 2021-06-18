import json
import logging
import os
from string import Template

from confluent_kafka import Consumer, Producer
from franz.openrdf.connect import ag_connect
from franz.openrdf.repository.repositoryconnection import RepositoryConnection
from franz.openrdf.rio.rdfformat import RDFFormat

from .model import Post

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
ch = logging.StreamHandler()
formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
ch.setLevel(logging.INFO)
ch.setFormatter(formatter)
logger.addHandler(ch)


AG_ENV = {
    "host": os.environ.get("AGRAPH_HOST", "localhost"),
    "port": os.environ.get("AGRAPH_PORT", "10035"),
    "user": os.environ.get("AGRAPH_USER", "test"),
    "password": os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
}


_RQ_TEMPLATE = Template(
    """PREFIX    : <http://franz.com/examples/>
PREFIX sna: <http://franz.com/ns/allegrograph/4.11/sna/>

SELECT DISTINCT ?user WHERE {
  ?group sna:egoGroup (:connectedPosts :$pid $depth) .
  ?post sna:members ?group ; :postedBy ?user .
  FILTER (?post != :$pid)
} LIMIT $limit"""
)


def recommend_users(ag_conn: RepositoryConnection, post: Post, depth=6, limit=10):
    data = {"pid": post.pid, "posted_by": post.posted_by, "recommend_users": []}
    query = _RQ_TEMPLATE.substitute(pid=post.pid, depth=depth, limit=limit)
    with ag_conn.prepareTupleQuery(query=query).evaluate() as res:
        for binding_set in res:
            user_uri = binding_set.getValue("user")
            data["recommend_users"].append(user_uri.localname)
    return data


def delivered_cb(err, msg):
    if err is not None:
        logger.error("Message delivery failed: {}".format(err))
    else:
        logger.info("Message delivered to topic <{}>".format(msg.topic()))


def main():
    ag_conn = ag_connect(os.environ["AGRAPH_REPO"], create=False, clear=False, **AG_ENV)
    ag_conn.add_commit_size = 1000
    logger.info(
        "AllegroGraph connected. ADD_COMMIT_SIZE: {}".format(ag_conn.add_commit_size)
    )

    producer = Producer({"bootstrap.servers": "localhost:9092"})

    consumer = Consumer(
        {
            "bootstrap.servers": "localhost:9092",
            "group.id": "agStreamingDemo",
            "auto.offset.reset": "latest",
        }
    )
    consumer.subscribe(["posts"])
    logger.info("Topic <posts> subscribed.")

    while True:
        msg = consumer.poll(1.0)
        if msg is None:
            continue
        if msg.error():
            logger.error("Consumer error: {}".format(msg.error()))
            continue

        post = Post(**json.loads(msg.value().decode("utf-8")))
        logger.info("Message of {} consumed.".format(post))

        # 1. Sink
        ag_conn.addData(
            post.to_nt(base="http://franz.com/examples/"), rdf_format=RDFFormat.NTRIPLES
        )
        logger.info(
            "NTriples data added for {}. Repo size: {}.".format(post, ag_conn.size())
        )
        # 2. Recommend
        data = recommend_users(ag_conn, post)
        if data["recommend_users"]:
            producer.produce(
                "recommendations",
                json.dumps(data).encode("utf-8"),
                callback=delivered_cb,
            )
            logger.info(
                "User recommendations of {} have been sent to topic <{}>".format(
                    post, "recommendations"
                )
            )

    # consumer.close()
