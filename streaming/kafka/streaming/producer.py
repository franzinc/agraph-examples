import logging
import os
import random
import time

from confluent_kafka import Producer

from .model import Post, load_sample_idx

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
ch = logging.StreamHandler()
formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
ch.setLevel(logging.INFO)
ch.setFormatter(formatter)
logger.addHandler(ch)


def delivered_cb(err, msg):
    if err is not None:
        logger.error("Message delivery failed: {}".format(err))
    else:
        logger.info("Message delivered to topic <{}>".format(msg.topic()))


def main():
    sample_users_idx = load_sample_idx("sample_users_idx.txt.bz2")
    sample_posts_idx = load_sample_idx("sample_posts_idx.txt.bz2")
    producer = Producer({"bootstrap.servers": "localhost:9092"})
    topic = "posts"
    limit = int(os.environ.get("NUM_POSTS", 100))
    for _ in range(0, limit):
        producer.poll(0)
        post = Post.generate(sample_users_idx, sample_posts_idx)
        data = post.to_json().encode("utf-8")
        producer.produce(topic, data, callback=delivered_cb)
        time.sleep(random.randint(1, 3))
