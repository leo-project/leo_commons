#!/bin/sh

rm -rf doc/rst && mkdir doc/rst
make doc
pandoc --read=html --write=rst doc/leo_date.html -o doc/rst/leo_date.rst
pandoc --read=html --write=rst doc/leo_file.html -o doc/rst/leo_file.rst
pandoc --read=html --write=rst doc/leo_hashtable.html -o doc/rst/leo_hashtable.rst
pandoc --read=html --write=rst doc/leo_hex.html -o doc/rst/leo_hex.rst
pandoc --read=html --write=rst doc/leo_http.html -o doc/rst/leo_http.rst
pandoc --read=html --write=rst doc/leo_math.html -o doc/rst/leo_math.rst
pandoc --read=html --write=rst doc/leo_mime.html -o doc/rst/leo_mime.rst
pandoc --read=html --write=rst doc/leo_misc.html -o doc/rst/leo_misc.rst
pandoc --read=html --write=rst doc/leo_mnesia.html -o doc/rst/leo_mnesia.rst
