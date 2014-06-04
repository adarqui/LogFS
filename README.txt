This is the LogFS library only.

building:

 cabal-meta install

example:

 mkdir /tmp/logfs
 ./dist/*/build/simple/simple 50 -f -o allow_other -o auto_unmount -o nonempty -o intr -o big_writes /tmp/logfs
 echo "hi" > /tmp/logfs/test
 ls -al /tmp/logfs*.log
