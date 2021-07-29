#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#
# This script performs some checks on the system
# it was initialy used to check an option (ignore_nice_load) on debian systems.
#
# cronjob:
#    .put this into "/etc/cron.daily/" as root (chmod u+x)
#   or
#    . set EDITOR=vim
#    . crontab -e
#    . add the line "0 0 * * * (path)/syschecker.py >/dev/null 2>&1"
#
# see http://www.debian-administration.org/articles/56

import os
import sys
import os.path
import socket
import smtplib
import time


class SysChecker:
    def __init__(self, watchers):
        self.watchers = watchers.split(';')
        self.report = ""

    def __mailmsg(self, toaddr, subject="no subject", file=None, text=None):
        """ send an e-mail
        """
        fromaddr = "%s@%s" % (os.path.basename(
            sys.argv[0]), socket.gethostbyaddr(socket.gethostname())[0])
        head = "From: %s\nTo: %s\nSubject: %s\n\n\n" % (
            fromaddr, toaddr, subject)
        if text == None:
            text = ""
        if file:
            try:
                f = open(file, 'r')
                text = f.read()
                f.close()
            except:
                text = "file not found"
        server = smtplib.SMTP("smtp.ulg.ac.be")
        server.sendmail(fromaddr, toaddr, head+text)
        server.quit()

    def __checkignorenice(self):
        """ check ignore_nice_load
        """
        message = 'checking ignore_nice_load on %s at %s\n' % (
            socket.gethostbyaddr(socket.gethostname())[0], time.asctime())
        fname = '/sys/devices/system/cpu/cpu0/cpufreq/ondemand/ignore_nice_load'
        message += 'file="%s"\n' % fname
        file = open(fname, 'r')
        val = file.read()
        file.close()
        vali = int(val)
        message += "value=%d\n" % vali
        if vali != 0:
            message += "WARNING: ignore_nice_load should be set to '0'!\n\n"
        else:
            message += "OK!\n"
        self.__mailmsg(
            self.watchers, subject="checkignorenice()", text=message)

    def execute(self):
        self.report = ""
        self.__checkignorenice()
        if self.report:
            self.__mailmsg(
                self.watchers, subject="SysChecker Report", text=self.report)


# -------------------------------------------------------


if __name__ == "__main__":
    print("running cronjob...")
    checker = SysChecker("r.boman@ulg.ac.be;l.papeleux@ulg.ac.be")
    #checker = SysChecker("r.boman@ulg.ac.be")
    checker.execute()
