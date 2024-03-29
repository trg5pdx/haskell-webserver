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

You can also provide a hostname and port from the commandline, although
the program will close if you provide incorrect arguments:

```
stack run localhost 3000
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

## Networking

Contains a typeclass for defining networking related functions. Mainly this was done
for the sake of simplifying network functions to only need a socket to run. It could
also be used for mock testing the network functionality, but that hasn't been done
for this project.

## Map

This module is meant for handling all data in the server that the user will be
getting data from, or putting their data into. Data is stored into a key/value
map that only stays in memory and doesn't write to file, so if the server will
to go down, it would lose everything that was inserted the last time it was
ran. This module contains functions for initializing a map, and functions for
getting data from it with a key, or inserting data with a key, value, and a
type.

## Parse

This module's main purpose is to take the HTTP messages sent in from the client,
interpret them, and then send back an appropriate response. This can involve sending
back a value if theres one in the map, returning that they successfully put a value
onto the map at a key, or returning an error. There are a few different errors that
can occur. It can send back a "Not found" error in the case where no value was found
associated with the key the client provided, or send back a "bad request" error which
occurs when an invalid request was sent into the server. In the latter case, the server
sends back a message telling the user what part it had issues with.

This module was split up to have an internal version containing helper
functions to reduce the overall size of functions. This internal module is only
used within Parse and in the testing suite, and these functions are expected to
be ran only from within the main parser functions.

## Reflection

Overall, I think the project went alright. I ran into a few different roadblocks, with
the biggest one being the trouble I had with getting new clients to see changes to the
map. For that one, I ended up going with the easiest route possible and just set up the
server to close the connection after sending back a response, so that way the calling
structure of the program would be linear rather than be a tree. This also lead to the
problem of a client never sending a command, which would then block other clients from
connecting, but that was fixed by using a timeout.

After I got the project mostly working, I went on to refactor it, and improve on the
code already there to get it to do more of what I wanted. This was difficult as I
would run into issues with how I should approach splitting apart larger functions,
or how I should rename functions/types/variables to make it more clear what they do,
or how to change my defined types to make more sense with newer types I introduced.
I did run into the problem of creating Response and ParseState to essentially be
tuples with 4 different values within it, which I fear that could be confusing to
someone reading what I've done without knowing what my types are/mean. I tried to
look into naming individual values within a type since I thought I saw it somewhere,
but from what I can tell it isn't acutally possible.

Another big issue I ran into was how to deal with parsing. At first, I took a lazy
approach where I essentially ignored all other data besides the first two words in a
message, but I started to run into issues with it when I wanted to send PUT requests
to the server. It took a while to work it out, but I came up with a solution that
processes each packet by line, and then set up a state to keep track of things as it
iterated through the lines of the request. While I like this way better as it allowed
for me to send multiline PUT requests/send in a valid html file, I think it
could be better. I've at least split up the functionality into multiple functions,
but I still feel that it could be improved upon. I've implemented a parser for
multiline packets before, but that was in python. While I was working on this parser,
I kept thinking back to that one I wrote, which also led me to try to approach it in
a more iterative way, which I think caused issues for me.

## Resources I used

Below are some resources I used while working on this project.

The code I used to kickstart developing the web server came from an example on the
network package page [here](https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html)

Got the minimum viable HTTP response packet from [here](https://stackoverflow.com/questions/33784127/minimal-http-server-reply)

Used this for figuring out how to move my parser helper functions into an internal
module to keep those functions from being exposed, while allowing for it to be
testable [here](https://lhbg-book.link/03-html/07-internal_modules.html)
