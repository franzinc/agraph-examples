import bz2
import json
import random
import uuid
from dataclasses import dataclass
from typing import Dict, List, Optional

import importlib_resources
from essential_generators import DocumentGenerator
from rdflib import Graph, Literal, Namespace
from rdflib.namespace import RDF, RDFS

from . import data as module_data

_GEN = DocumentGenerator()


def load_sample_idx(fname: str) -> List[str]:
    c = importlib_resources.read_binary(module_data, fname)
    data = bz2.decompress(c).decode("utf-8")
    return [l for l in data.split("\n") if l]


@dataclass
class User:
    uid: str
    name: str
    description: str

    def __hash__(self) -> int:
        return hash(self.uid)

    def __repr__(self) -> str:
        return f"<{self.uid}>"

    def to_json(self) -> Dict:
        data = {
            "uid": self.uid,
            "name": self.name,
            "description": self.description,
        }
        return json.dumps(data)

    def to_nt(self, base: str) -> str:
        ns = Namespace(base)
        u = ns[self.uid]
        g = Graph()
        g.add((u, RDF.type, ns["User"]))
        g.add((u, RDFS.label, Literal(self.name)))
        g.add((u, ns.name, Literal(self.name)))
        g.add((u, ns.description, Literal(self.description)))
        return g.serialize(format="nt").decode("utf-8")


@dataclass
class Post:
    pid: str
    content: str
    posted_by: str
    retweet_of: Optional[str]
    reply_to: Optional[str]

    def __hash__(self) -> int:
        return hash(self.pid)

    def __repr__(self) -> str:
        return f"<{self.pid}>"

    def to_json(self) -> Dict:
        data = {
            "pid": self.pid,
            "content": self.content,
            "posted_by": self.posted_by,
            "retweet_of": self.retweet_of if self.retweet_of else None,
            "reply_to": self.reply_to if self.reply_to else None,
        }
        return json.dumps(data)

    def to_nt(self, base: str) -> str:
        ns = Namespace(base)
        p = ns[self.pid]
        g = Graph()
        g.add((p, RDF.type, ns["Post"]))
        g.add((p, ns.content, Literal(self.content)))
        g.add((p, ns.postedBy, ns[self.posted_by]))
        if self.retweet_of:
            g.add((p, ns.retweetOf, ns[self.retweet_of]))
        if self.reply_to:
            g.add((p, ns.replyTo, ns[self.reply_to]))
        return g.serialize(format="nt").decode("utf-8")

    @staticmethod
    def generate(sample_users_idx: List[str], sample_posts_idx: List[str]) -> "Post":
        data = {
            "pid": f"post-{uuid.uuid4()}",
            "content": _GEN.sentence(),
            "posted_by": random.choice(sample_users_idx),
            "retweet_of": None,
            "reply_to": None,
        }
        prob = random.uniform(0, 1)
        if prob < 0.5:
            return Post(**data)
        elif prob < 0.75:
            data["retweet_of"] = random.choice(sample_posts_idx)
        else:
            data["reply_to"] = random.choice(sample_posts_idx)
        return Post(**data)
