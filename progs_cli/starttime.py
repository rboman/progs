#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# get the start time of a process given its PID
#
# usage:
#   python startTime.py $PID

import sys
import datetime
import time
import subprocess


def main():
    pid = sys.argv[1]
    proc = subprocess.Popen(['ps', '-eo', 'pid,etime'], stdout=subprocess.PIPE)
    # get data from stdout
    # proc.wait()
    # results = proc.stdout.readlines()
    out, err = proc.communicate()
    results = out.decode().strip().split('\n')

    # parse data (should only be one)
    for result in results:
        try:
            if result.split()[0] == pid:
                pidInfo = result.split()[1]
                # stop after the first one we find
                break
        except IndexError:
            pass  # ignore it
    else:
        # didn't find one
        print("Process PID", pid, "doesn't seem to exist!")
        sys.exit(0)

    pidInfo = [result.split()[1] for result in results if result.split()[0] == str(pid)][0]
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
    print("Process started on", end=' ')
    print(datetime.datetime.fromtimestamp(startTime).strftime("%a %b %d at %I:%M:%S %p"))


if __name__ == '__main__':
    main()
