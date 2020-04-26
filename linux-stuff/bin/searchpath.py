#!/usr/bin/env python3

import os
import sys

keyword = sys.argv[1]

path = os.getenv('PATH')
files = [os.listdir(d) for d in path.split(':') if os.path.exists(d)]
filesFlat = []
[filesFlat.extend(thing) for thing in files]

for f in filesFlat:
    if not f.find(keyword) == -1:
        print(f)
