#! /usr/bin/env python
# -*- coding: latin-1; -*-

import smtplib

fromaddr = "r.boman@ulg.ac.be"
toaddrs = "romain.boman@gmail.com"

head = "From: %s\nTo: %s\n\n" % (fromaddr, toaddrs)
body = "salut!\nRoBo\n"
msg = head+body

server = smtplib.SMTP("smtp.ulg.ac.be")
server.set_debuglevel(1)
server.sendmail(fromaddr, toaddrs, msg)
server.quit()
