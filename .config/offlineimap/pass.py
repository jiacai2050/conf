#!/usr/bin/python
import re
import os
import os.path


def get_password_emacs(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s\n" % (
        machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.config/authinfo.gpg").read()
    return p.search(authinfo).group(1)


prioritized = ['INBOX', 'Drafts', 'Junk', 'Sent', 'Sent Messages']


def mycmp(x, y):
    for prefix in prioritized:
        xsw = x.startswith(prefix)
        ysw = y.startswith(prefix)
        if xsw and ysw:
            return cmp(x, y)
        elif xsw:
            return -1
        elif ysw:
            return +1
    return cmp(x, y)


def test_mycmp():
    folders = os.listdir(os.path.expanduser('~/.mail/ljc'))
    folders.sort(mycmp)
    print folders
