# coding=utf-8
"""
mmterm python client
"""
import time
import cv
import numpy as np
import socket
import msgpack

class MMLog(object):
    def __init__(self):
        try:
            self.client = socket.create_connection(('127.0.0.1', 8001))
        except:
            print('log server not found; messaged will be dropped')
            self.client = None
    
    def _send(self, packet):
        if self.client==None:
            print('message is dropped')
        else:
            self.client.sendall(msgpack.packb(packet))
    
    def log(self, x):
        if type(x) is str:
            self._send(["str",x])
        elif type(x) is cv.iplimage or type(x) is cv.cvmat:
            if (type(x)==cv.iplimage and x.depth!=8) or (type(x)==cv.cvmat and  x.type!=cv.CV_8UC1 and x.type!=cv.CV_8UC3):
                self.log('unknown image format: %s'%x)
            else:
                self._send(["uni","u8",[x.height, x.width, x.channels], x.tostring()])
        elif type(x) is np.ndarray:
            if x.dtype!=np.uint8:
                self.log('unsupported array type: %s'%x.dtype)
            else:
                self._send(["uni", "u8", x.shape, x.tostring()])
        else:
            print('unknown format: %s'%type(x))

