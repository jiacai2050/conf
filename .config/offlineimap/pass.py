#!/usr/bin/python
import re
import os


def get_password_emacs(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s\n" % (
        machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.config/authinfo.gpg").read()
    return p.search(authinfo).group(1)
