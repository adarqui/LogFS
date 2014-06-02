*** EARLY DEVELOPMENT ***

porting logfs-c over to haskell. This will eventually (hopefully) become a solid production version of my old logfs prototype.

example (future) usage:

 ./logfs redis://password@localhost zmq://udp://somehost:5555 file:///var/log/logfs.log ::: -f

 etc

 logfs: Fuse mount for /var/log etc
 logfs-cli: Userland logger util.
 logfs-srv: LogFS server. This will support the various backends (redis, zmq, ...). It will also have its' own database backends (mysql, mongodb, sqlite3, flat file etc)


installation:
 cabal sandbox init
 cabal-meta install
 ./.cabal-sandbox/bin/logfs ...
