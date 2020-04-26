#!/usr/bin/env python3

import sys

def rot(string, num):
    return "".join(chr(ord(c) + num) for c in string)

def flip(string, num):
    return "".join(chr(255 - ord(c)) for c in string)

FUNC = rot

if __name__ == '__main__':
    if len(sys.argv) == 1:
        print(" ".join([sys.argv[0], 'file', 'num']))
        sys.exit(1)

    if len(sys.argv) == 2:
        for l in sys.stdin.readlines():
            print(FUNC(l.strip(), int(sys.argv[1])))
    else:
        print(FUNC(sys.argv[1], int(sys.argv[2])))
