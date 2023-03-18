# Haskell-webserver

This is a basic key-value webserver implemented in the Haskell programming
language. This server is meant for no serious use cases and is just an
exercise for me to work more in Haskell, and with functional programming
in general.

# Running the webserver

First, clone the repo by running the following command:

```
git clone https://github.com/trg5pdx/haskell-webserver
```

Afterwards, cd into haskell-webserver and run the following command
to start the server on localhost (port 4700):

```
stack run
```

If you want to run the tests written for the modules in this project,
you can run:

```
stack test
```

# Modules

This project contains three different modules for networking operations, with
the functions from those modules being ran in app/Main.hs. Below, there's some
information about the modules in this project:

# Networking

Contains a typeclass for defining networking related functions. Mainly this was done
for the sake of simplifying network functions to only need a socket to run. It could
also be used for mock testing the network functionality, but that hasn't been done
for this project.

# Map

This module contains the definitions for the map thats storing the keys/values, the
type that determines what value each value in the map is, and a definition for the
result of map operations. This module's main purpose is to initialize a map, obtain
data from the map using a key, and put data onto the map with a key, value, and a type.

# Parse

This module's main purpose is to take the HTTP messages sent in from the client,
interpret them, and then send back an appropriate response. This can involve sending
back a value if theres one in the map, returning that they successfully put a value
onto the map at a key, or returning an error. The errors return back not found in the
case of a user sending a key that isn't in the map, or the message was sent incorrectly,
so the server needs to inform clients that the message sent was wrong and why it
was wrong.

# Reflection

Overall, I think the project went alright. I ran into a few different roadblocks, with
the biggest one being the trouble I had with getting new clients to see changes to the
map. For that one, I ended up going with the easiest route possible and just set up the
server to close the connection after sending back a response, so that way the calling
structure of the program would be linear rather than be a tree. This also lead to the
problem of a client never sending a command, which would then block other clients from
connecting, but that was fixed by using a timeout.

Another big issue I ran into was how to deal with parsing. At first, I took a lazy
approach where I essentially ignored all other data besides the first two words in a
message, but I started to run into issues with it when I wanted to send PUT requests
to the server. It took a while to work it out, but I came up with a solution that
processes each packet by line, and then set up a state to keep track of things as it
iterated through the lines of the request. While I like this way better as it allowed
for me to send multiline PUT requests, and send in a valid html file, but I think it
could be better. I've at least split up the functionality into multiple functions, but
I still feel that it could be improved upon.
