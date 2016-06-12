#! /usr/bin/env python
# -*- coding: latin-1; -*-

import smtplib
from email.MIMEText import MIMEText

file = open("CYGWIN-diffs.html",'r')
text = file.read()
file.close()

toA = "r.boman@ulg.ac.be"
fromA = "r.boman@ulg.ac.be"
mail = MIMEText(text)
mail['From'] = fromA
mail['Subject'] = "Sujet du message"
mail['To'] = toA
mail['Content-Type'] = "text/html"

smtp = smtplib.SMTP("smtp.ulg.ac.be")
smtp.set_debuglevel(1)
smtp.sendmail(fromA, [toA], mail.as_string())
smtp.close()
    