#!/usr/bin/env python
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

import sys
import datetime
import time
import subprocess

# call like this: python startTime.py $PID

pid = sys.argv[1]
proc = subprocess.Popen(['ps', '-eo', 'pid,etime'], stdout=subprocess.PIPE)
# get data from stdout
proc.wait()
results = proc.stdout.readlines()
# parse data (should only be one)
for result in results:
    try:
        result.strip()
        if result.split()[0] == pid:
            pidInfo = result.split()[1]
            # stop after the first one we find
            break
    except IndexError:
        pass  # ignore it
else:
    # didn't find one
    print "Process PID", pid, "doesn't seem to exist!"
    sys.exit(0)
pidInfo = [result.split()[1] for result in results
           if result.split()[0] == pid][0]
pidInfo = pidInfo.partition("-")
if pidInfo[1] == '-':
    # there is a day
    days = int(pidInfo[0])
    rest = pidInfo[2].split(":")
    hours = int(rest[0])
    minutes = int(rest[1])
    seconds = int(rest[2])
else:
    days = 0
    rest = pidInfo[0].split(":")
    if len(rest) == 3:
        hours = int(rest[0])
        minutes = int(rest[1])
        seconds = int(rest[2])
    elif len(rest) == 2:
        hours = 0
        minutes = int(rest[0])
        seconds = int(rest[1])
    else:
        hours = 0
        minutes = 0
        seconds = int(rest[0])

# get the start time
secondsSinceStart = days * 24 * 3600 + hours * 3600 + minutes * 60 + seconds
# unix time (in seconds) of start
startTime = time.time() - secondsSinceStart
# final result
print "Process started on",
print datetime.datetime.fromtimestamp(startTime).strftime("%a %b %d at %I:%M:%S %p")
