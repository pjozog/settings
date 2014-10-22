#!/usr/bin/env python
"""Python 2.5 script. Creates a Notification pop-up bubble"""
import dbus
import sys, os

scriptdir = os.path.dirname (os.path.realpath(__file__))

APP_NAME = 'Test Application'
ITEM = "org.freedesktop.Notifications"
PATH = "/org/freedesktop/Notifications"
INTERFACE = "org.freedesktop.Notifications"
ID_NUM_TO_REPLACE = 0
ACTIONS_LIST = ''

class Urgency:
    NONE = 0
    IMPORTANT = 1
    URGENT = 2

# time is seconds x 1000
def notify (title, text, urgency=Urgency.IMPORTANT, time=3000):
    if urgency == Urgency.URGENT:
        icon = os.path.join (scriptdir, "application-exit.png")
    else:
        icon = os.path.join (scriptdir, "dialog-ok-apply.png")
    hint = {'urgency': dbus.Byte (urgency)}

    bus = dbus.SessionBus ()
    notif = bus.get_object (ITEM, PATH)
    notify = dbus.Interface (notif, INTERFACE)
    notify.Notify (APP_NAME, ID_NUM_TO_REPLACE, icon, title, text, ACTIONS_LIST, hint, time)

if __name__ == '__main__':

    if len (sys.argv) != 4:
        print 'Usage: %s [title] [text] [urgency (0, 1, or 2)]' % os.path.basename (sys.argv[0])
        sys.exit (-1)

    title = sys.argv[1]
    text = sys.argv[2]
    urgency = int (sys.argv[3])
    notify (title, text, urgency)
