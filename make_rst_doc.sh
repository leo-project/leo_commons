#!/bin/sh

make doc
rm -rf doc/rst && mkdir doc/rst

for Mod in leo_date \
           leo_file \
           leo_hashtable \
           leo_hex \
           leo_http \
           leo_math \
           leo_mime \
           leo_misc \
           leo_mnesia
do
    read_file="doc/$Mod.html"
    write_file="doc/rst/$Mod.rst"

    pandoc --read=html --write=rst "$read_file" -o "$write_file"

    sed -ie "1,6d" "$write_file"
    sed -ie "1s/\Module //" "$write_file"
    LINE_1=`cat $write_file | wc -l`
    LINE_2=`expr $LINE_1 - 10`
    sed -ie "$LINE_2,\$d" "$write_file"
done
rm -rf doc/rst/*.rste
