# -*- coding: utf-8 -*-
string='𝙱𝚘𝚗 𝚊𝚗𝚗𝚒𝚟 𝙾𝚕𝚒𝚟𝚒𝚎𝚛'
print (repr(string.encode()))
# print('\u0420\u043e\u0441\u0441\u0438\u044f')
# print ('\u0420\u043e\u0441\u0441\u0438\u044f'.decode())
a = b'\xf0\x9d\x99\xb1\xf0\x9d\x9a\x98\xf0\x9d\x9a\x97 \xf0\x9d\x9a\x8a\xf0\x9d\x9a\x97\xf0\x9d\x9a\x97\xf0\x9d\x9a\x92\xf0\x9d\x9a\x9f \xf0\x9d\x99\xbe\xf0\x9d\x9a\x95\xf0\x9d\x9a\x92\xf0\x9d\x9a\x9f\xf0\x9d\x9a\x92\xf0\x9d\x9a\x8e\xf0\x9d\x9a\x9b'
print((''.encode('ascii', 'backslashreplace')))
b = '\U0001d671\U0001d698\U0001d697 \U0001d68a\U0001d697\U0001d697\U0001d692\U0001d69f \U0001d67e\U0001d695\U0001d692\U0001d69f\U0001d692\U0001d68e\U0001d69b'
print (b)