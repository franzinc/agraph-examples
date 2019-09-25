#! /usr/bin/env python3

# Convert Bitcoin chain data to triples that can be loaded into a
# RDF-enabled graph database.
#
# Target platform is AllegroGraph but any sufficiently powerful triple
# store can be used.

import time
import sys
import argparse
import multiprocessing
from urllib.parse import urlparse
from http.client import RemoteDisconnected, BadStatusLine

import bitcoin
import bitcoin.rpc
from bitcoin.core import b2x, b2lx

from franz.openrdf.connect import ag_connect
from franz.openrdf.vocabulary.xmlschema import XMLSchema as XSD
from franz.openrdf.vocabulary import RDF


BTC_RDF_DATA = 'bitcoin://'
BTC_RDF_MODEL = ('https://raw.githubusercontent.com/'
                 'franzinc/agraph-examples/master/data/bitcoin/model.ttl#')


class ChainSupplier:
    """Simple interface to current chain data and updates."""

    def __init__(self, node, height, testnet, end_height, step):
        bitcoin.SelectParams('testnet' if testnet else 'mainnet')
        self._node = node
        self._connection = bitcoin.rpc.Proxy(service_url=self._node)
        self._startblock = height
        self._endblock = end_height
        self._current_block = self._startblock
        self._step = step

    def __iter__(self):
        self._current_block = self._startblock
        return self

    def __next__(self):
        # Loop trying to read the current block every 10 seconds and
        # return the read block, increasing the current block index.
        index = self._current_block
        while self._endblock is None or self._current_block <= self._endblock:
            try:
                hash = self._connection.getblockhash(index)
                block = self._connection.getblock(hash)
                break
            # Invalid parameter error probably means we are
            # synchronized, so wait for new blocks to come.
            except bitcoin.rpc.InvalidParameterError:
                print('waiting for block {}'.format(index))
                sys.stdout.flush()
                time.sleep(500)
                continue
            # On disconnect exception, immediately reconnect and continue.
            except RemoteDisconnected as e:
                self._connection = bitcoin.rpc.Proxy(service_url=self._node)
                continue
            # On any other HTTP exception, report error, reconnect and
            # continue.
            except Exception as e:
                fmtstr = 'error retrieving block {} ({}), reconnecting'
                print(fmtstr.format(index, type(e)))
                sys.stdout.flush()
                time.sleep(10)
                self._connection = bitcoin.rpc.Proxy(service_url=self._node)
                continue
        self._current_block += self._step
        return (index, block)


class ChainEater:
    """Consume chain data produced by ChainSupplier.

    Initialize AllegroGraph triple store, load Bitcoin chain data model and
    chain data itself converting it to triples.

    """

    def __init__(self, chainsupplier, instance, dbname, clear):
        # Perform necessary setup by connecting to the repository, creating a
        # URI factory and setting the provided supplier.
        url = urlparse(instance)
        self._dbname = dbname
        self._connargs = {
            'host': url.hostname,
            'port': url.port,
            'user': url.username,
            'password': url.password,
            'create': True,
            'clear': clear,
            'autocommit': True,
        }
        self._connection = ag_connect(self._dbname, **self._connargs)
        self._connection.setAddCommitSize(10000)
        self._mns = self._connection.namespace(BTC_RDF_MODEL)
        self._dns = self._connection.namespace(BTC_RDF_DATA)
        self._supplier = chainsupplier
        if clear:
            # Drop unused indices.
            for i in ["gpsoi", "gposi", "gospi", "gopsi", "gspoi"]:
                self._connection.dropIndex(i)
            # Load model into repository.
            self._connection.setNamespace('btcm', BTC_RDF_MODEL)
            self._connection.setNamespace('btcd', BTC_RDF_DATA)
            self._connection.addFile('model.ttl')

    def start(self):
        # Start consumption process.
        self.convert_blocks()

    def convert_blocks(self):
        self._triples = list()
        for index, block in self._supplier:
            # Convert block and buffer triples.
            self.convert_block(index, block)
            # Write out triple buffer to AG repository.
            self._connection.addTriples(self._triples)
            self._triples = list()

    def literal(self, value, datatype=XSD.STRING):
        """Create new literal of the specified type."""
        return self._connection.createLiteral(value, datatype=datatype)

    def model_uri(self, name):
        """Generate new URI `name` in `BTC_RDF_MODEL` namespace."""
        return getattr(self._mns, name)

    def data_uri(self, name):
        """Generate new URI `name` in `BTC_RDF_DATA` namespace."""
        return getattr(self._dns, name)

    def _add(self, *triples):
        self._triples += triples

    def convert_block(self, index, block):
        """
        Return list of triples representing Bitcoin block according to the
        model specified in model.ttl.
        """
        # Curry few heavily used methods to make intents clearer.
        m, d, v, add = self.model_uri, self.data_uri, self.literal, self._add
        # Extract and normalize raw block data.
        blk_node = 'block{}'.format(index)
        blk_hash = b2lx(block.GetHash())
        blk_version = block.nVersion
        blk_time = block.nTime
        # Add block header data.
        add((d(blk_node), RDF.TYPE, m('Block')),
            (d(blk_node), m('height'), v(index, XSD.INT)),
            (d(blk_node), m('hash'), v(blk_hash, XSD.STRING)),
            (d(blk_node), m('version'), v(blk_version, XSD.INT)),
            (d(blk_node), m('time'), v(blk_time, XSD.INT)))
        # Add previous block links.
        if index > 0:
            blk_prev_blk_node = d('block{}'.format(index-1))
            add((d(blk_node), m('prevBlock'), blk_prev_blk_node))
        # Add transaction data.
        for tx_i, tx in enumerate(block.vtx):
            tx_node = b2lx(tx.GetHash())
            tx_locktime = tx.nLockTime
            add((d(tx_node), RDF.TYPE, m('Transaction')),
                (d(tx_node), m('index'), v(tx_i, XSD.INT)),
                (d(tx_node), m('txid'), v(tx_node, XSD.STRING)),
                (d(tx_node), m('lockTime'), v(tx_locktime, XSD.LONG)),
                (d(blk_node), m('transaction'), d(tx_node)))
            # Add txin data.
            for in_i, txin in enumerate(tx.vin):
                txin_node = self._connection.createBNode()
                output_hash = b2lx(txin.prevout.hash)
                output_n = txin.prevout.n
                script = self.decode_script(txin.scriptSig)
                add((txin_node, RDF.TYPE, m('TxInput')),
                    (txin_node, m('index'), v(in_i, XSD.INT)),
                    (txin_node, m('outputHash'), v(output_hash, XSD.STRING)),
                    (txin_node, m('outputIndex'), v(output_n, XSD.STRING)),
                    (txin_node, m('unlockScript'), v(script, XSD.STRING)),
                    (d(tx_node), m('input'), txin_node))
            # Add txout data.
            for out_i, txout in enumerate(tx.vout):
                txout_node = self._connection.createBNode()
                amount = txout.nValue
                script = self.decode_script(txout.scriptPubKey)
                add((txout_node, RDF.TYPE, m('TxOutput')),
                    (txout_node, m('index'), v(out_i, XSD.INT)),
                    (txout_node, m('amount'), v(amount, XSD.LONG)),
                    (txout_node, m('lockScript'), v(script, XSD.STRING)),
                    (d(tx_node), m('output'), txout_node))

    def decode_script(self, script):
        """Decode Bitcoin script into a human readable string.

        Adapted from bitcoin.core.script.CScript.__repr__ method.
        """
        ops = []
        try:
            for o in script:
                ops.append(b2x(o) if isinstance(o, bytes) else repr(o))
        except bitcoin.core.script.CScriptTruncatedPushDataError as e:
            ops.append('{}...<ERROR: {}>'.format(b2x(e.data), e))
        except bitcoin.core.script.CScriptInvalidError as e:
            ops.append('<ERROR: {}>'.format(e))
        return ' '.join(ops)


def load_bitcoin(*args):
    """Entry point for each worker process.

    Creates its own chain supplier and chain eater objects and loads
    Bitcoin blocks with indexes (height + i*workers).

    """
    queue, source, dest, height, testnet, name, end_height, workers = args
    # Queue is used to ensure workers start sequentially. See comments
    # in __main__.
    clear = queue.get()
    eater = ChainEater(
        ChainSupplier(source, height, testnet, end_height, workers),
        dest, name, clear
    )
    queue.put(False)
    eater.start()


class ChainStats:
    def __init__(self, node):
        self._node = node
        self._connection = bitcoin.rpc.Proxy(service_url=self._node)

    def _poll(self, f):
        while True:
            try:
                return f()
            except Exception as e:
                fmtstr = 'error reading chain stats ({}), reconnecting'
                print(fmtstr.format(e))
                sys.stdout.flush()
                time.sleep(10)
                self._connection = bitcoin.rpc.Proxy(service_url=self._node)
                continue

    def blockcount(self):
        def f(): return self._connection.getblockcount()
        return self._poll(f)

    def txcount(self):
        def f(): return self._connection._call('getchaintxstats')['txcount']
        return self._poll(f)


class AGStats:
    def __init__(self, instance, dbname, clear):
        url = urlparse(instance)
        self._dbname = dbname
        self._connargs = {
            'host': url.hostname,
            'port': url.port,
            'user': url.username,
            'password': url.password,
            'create': True,
            'clear': clear,
        }
        self._connection = ag_connect(self._dbname, **self._connargs)

    def _count_query(self, query):
        for binding_set in self._connection.executeTupleQuery(query):
            return binding_set['count'].decimalValue()
        return 0

    def blockcount(self):
        return self._count_query(
            'SELECT (COUNT(*) AS ?count) WHERE {?tx a btcm:Block.}'
        )

    def txcount(self):
        return self._count_query(
            'SELECT (COUNT(*) AS ?count) WHERE {?tx a btcm:Transaction.}'
        )


def log_bitcoin(*args):
    """Entry point for logger process."""
    queue, source, dest, name = args
    # Queue is used to ensure workers start sequentially. See comments
    # in __main__.
    clear = queue.get()
    chain = ChainStats(source)
    ag = AGStats(dest, name, clear)
    while True:
        our_blockcount = ag.blockcount()
        their_blockcount = chain.blockcount()
        our_txcount = ag.txcount()
        their_txcount = chain.txcount()
        pc = our_txcount / their_txcount
        print('loading: {:1.8f} ({} of {} blocks)'.format(
            pc, our_blockcount, their_blockcount))
        sys.stdout.flush()
        time.sleep(20)


class HelpFormatter(argparse.ArgumentDefaultsHelpFormatter,
                    argparse.RawDescriptionHelpFormatter):
    """Print default values for arguments.
    Do not touch description and epilog."""
    pass


if __name__ == '__main__':
    description = ('convert Bitcoin chain data into triple-based '
                   'representation and load it into an AG repository.')
    parser = argparse.ArgumentParser(
        description=description,
        formatter_class=lambda prog: HelpFormatter(
            prog, max_help_position=50, width=100),
        epilog=('''example:

  # load data from Bitcoin node at localhost:8332 into
  # an AG repository named \'bitcoin\' running on AG
  # server at locahost:10035, using 4 worker processes

  ./convert.py \\
        --source=http://btcuser:btcpass@localhost:8332 \\
        --destination=http://aguser:agpass@localhost:10035 \\
        --name=bitcoin \\
        --workers=4 \\
        --clear'''))

    btcgroup = parser.add_argument_group('Bitcoin arguments')
    btcgroup.add_argument('--source',
                          help='Bitcoin node which will supply the data')
    btcgroup.add_argument('--start-height',
                          default=0,
                          help=('start loading from given block heights; '
                                'can be a comma-separated list for each '
                                'worker thread'))
    btcgroup.add_argument('--end-height',
                          default=None,
                          help='finish loading at a given block height')
    btcgroup.add_argument('--testnet',
                          action='store_true',
                          help='Use testnet if set')
    btcgroup.add_argument('--workers',
                          default=4,
                          help=('number of worker processes to spawn; '
                                'each worker will be loading blocks with a '
                                'step that equals to the number of workers'))

    aggroup = parser.add_argument_group('AG arguments')
    aggroup.add_argument('--destination',
                         metavar='DEST',
                         help='AG instance which will consume the data')
    aggroup.add_argument('--name',
                         default='bitcoin',
                         help='name of the repository to store data')
    aggroup.add_argument('--clear',
                         action='store_true',
                         help='clear out the database if it exists')

    args = parser.parse_args()

    source = args.source
    dest = args.destination
    testnet = args.testnet
    name = args.name
    workers = int(args.workers)
    end_height = None if args.end_height is None else int(args.end_height)

    try:
        base_height = int(args.start_height)
        heights = [base_height + i for i in range(workers)]
    except ValueError:
        heights = [int(h) for h in args.height.split(',')]

    assert len(heights) == workers, "Each worker's height must be specified."

    # Queue is used to determine the process which will clear the
    # repository: if the user specifies --clear on the command line,
    # the first entry in the queue will be True, and the winning
    # process will take it and clear the database. Each worker puts
    # the value False into the queue, allowing the next worker to
    # start (see `load_bitcoin()` and `log_bitcoin()` functions).
    queue = multiprocessing.Queue()
    queue.put(args.clear)

    processes = []

    for i in range(workers):
        pargs = (queue, source, dest, heights[i], testnet, name, end_height,
                 workers)
        process = multiprocessing.Process(target=load_bitcoin, args=pargs)
        process.start()
        processes.append(process)

    pargs = (queue, source, dest, name)
    process = multiprocessing.Process(target=log_bitcoin, args=pargs)
    process.start()
    processes.append(process)

    for p in processes:
        p.join()
