#!/bin/bash 

if [ -e site ]
then
  rsync -r --delete ./site/* ../danielbarter.github.io/
else
  echo 'you need to build the site first'
fi
