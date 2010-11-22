#!/usr/bin/env python

import os
import sys

keyword = sys.argv[1]

path = os.getenv('PATH')
files = [os.listdir(d) for d in path.split(':')]
filesFlat = []
[filesFlat.extend(thing) for thing in files]

for f in filesFlat:
    if not f.find(keyword) == -1:
        print f
