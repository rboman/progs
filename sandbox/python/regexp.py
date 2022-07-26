
import re

line = "[TSC-FAILED]"
#line = "[TSC-FAILED] Hello"
reg = re.compile(r'\[TSC-FAILED\]\s*(.+)?')
match = reg.match(line)

print(f'match={match}')

if match:
    # optional group produce 'None'
    print (match.groups())
else:
    print('no match')
    