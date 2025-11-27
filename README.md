# literate-broccoli
Docker Log Monitor for Localhost

This is a very simple Haskell Script that runs on an infinite loop. It listens for "ml-service" and if it doesn't have
a heartbeat, it'll alert the user.

Extensions in the future 

# How to Run

Running is simple. Just write the command `./monitor.hs`. It should install the compiler, linker, and packages if they
are not already available on your machine. It looks across all the containers running on you machine currently.
