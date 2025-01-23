# -*- coding: utf-8 -*-
# pylint: disable=C0111,C0103,R0205

import argparse
import logging
import pika
import pika.exceptions
import queue

CONSUMER_TAG="RMQ-1280-CTAG"
QUEUE="RMQ-1280-0"

log_fmt = "%(asctime)s.%(msecs)03d %(levelname)s %(message)s"
log_date_fmt = "%Y-%m-%d %H:%M:%S"
logging.basicConfig(level=logging.INFO, format=log_fmt, datefmt=log_date_fmt)
logger = logging.getLogger()

parser = argparse.ArgumentParser(
    prog="repro.py", description="attempt to repro rmq-1280", add_help=False
)
parser.add_argument("-h", "--host", default="shostakovich")
parser.add_argument("-p", "--port", default="5672", type=int)
parser.add_argument("-v", "--vhost", default="/")
ns = parser.parse_args()
rmq_port = ns.port
rmq_vhost = ns.vhost
rmq_host = ns.host

credentials = pika.PlainCredentials("guest", "guest")
parameters = pika.ConnectionParameters(
    host=rmq_host,
    port=rmq_port,
    virtual_host=rmq_vhost,
    credentials=credentials,
)

global acks
acks = queue.SimpleQueue()

global connection
connection = pika.BlockingConnection(parameters)

channel = connection.channel()
channel.basic_qos(prefetch_count=25)


def on_message(ch, method_frame, header_frame, body):
    delivery_tag = method_frame.delivery_tag
    logger.info("received message with dtag: %d", delivery_tag)
    if delivery_tag % 100 == 0:
        dtag = 0
        if acks.empty():
            logger.info("NOT acking message with dtag: %d", delivery_tag)
            acks.put(delivery_tag)
        else:
            dtag = acks.get()
            logger.info("NOT acking message with dtag: %d, acking previous tag: %d", delivery_tag, dtag)
            ch.basic_ack(dtag)
            acks.put(delivery_tag)
        connection.process_data_events(1)
        logger.info("restarting consumer")
        ch.basic_cancel(CONSUMER_TAG)
        connection.process_data_events(1)
        ch.basic_consume(on_message_callback=on_message, consumer_tag=CONSUMER_TAG, queue=QUEUE)
    else:
        logger.info("acking message with dtag: %d", delivery_tag)
        ch.basic_ack(delivery_tag)


channel.basic_consume(on_message_callback=on_message, consumer_tag=CONSUMER_TAG, queue=QUEUE)

try:
    channel.start_consuming()
except pika.exceptions.ChannelClosedByBroker as ccbex:
    logger.error("channel exception: %s", ccbex)
except KeyboardInterrupt:
    channel.stop_consuming()

connection.close()
