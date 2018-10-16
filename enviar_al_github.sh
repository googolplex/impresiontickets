#!/bin/bash
# hecho el 2018.10.15a
cp ./fuentes/*.prg ./impresiontickets/
cp ./fuentes/*.txt ./impresiontickets/
cp ./*.sh ./impresiontickets/
cd impresiontickets
git add -A
git commit -a -m $1
git push -u origin master
