#!/usr/bin/env python3
# -*- coding: utf8 -*-

from urllib.request import urlretrieve
import os

target_folder = r'D:\dev\Metafor\oo_metaB\bin\Release'
mesa3d_archive_url = r'https://github.com/pal1000/mesa-dist-win/releases/download/23.1.6/mesa3d-23.1.6-release-msvc.7z'

def download_file(url_file):
    filename = os.path.basename(url_file)
    import urllib.request

    response = urllib.request.urlopen(url_file)
    data = response.read()

    with open(filename, "wb") as file:
        file.write(data)

    # baseurl = os.path.dirname(url_file)
    # filename = os.path.basename(url_file)
    # print(baseurl)
    # print(filename)
    # url = ( baseurl, filename )
    # path, headers = urlretrieve(url, filename)
    # print('path =', path)
    # print('headers =', headers)
    # print()
    # for name, value in headers.items():
    #     print(name, value)


# >>> url = (
# ...     "https://api.worldbank.org/v2/en/indicator/"
# ...     "NY.GDP.MKTP.CD?downloadformat=csv"
# ... )
# >>> filename = "gdp_by_country.zip"

# path, headers = urlretrieve(url, filename)
# >>> for name, value in headers.items():
# ...     print(name, value)

if __name__=="__main__":
    download_file(mesa3d_archive_url)
