Development Log
===============

(All times CDT)

Fri 7/6 7:06 PM
---------------

Started a README.md and this file. Getting setup to start hacking on
Spawnfest. Beer and soda are in the fridge. I have money to order food
when I get hungry. Looking good so far.

Next step is to write up a simple demonstration for the issue that I'm
intending to address with these wrappers.

Fri 7/6 7:26 PM
---------------

Managed to get the test up and running easily enough. Can be run like such:

    ./test/example-problem.es
          10 ::  1.28s
        1000 ::  1.37s
       10000 ::  1.93s
      100000 ::  7.51s
     1000000 :: 81.33s

These times are from running locally on a 13-inch, Late 2010 MacBook Air.
The important point to notice is that's an SSD. While it should behave
reasonably similar on an HDD the raw times aren't what's important. Each
line there writes a 10MiB file in 4KiB chunks and then reads it back in
4KiB chunks.

The only difference here is that we've loaded the message queue of the
process reading and writing data with a number of messages (10-1M). The
presence of these messages in the message queue hurts because of a selective
receive in Erlang's [file handling][file_handling].

My original entry was going to be to try and apply the make\_ref()
selective receive optimization but that turned out to be a lot of
code upheaval inside the file driver.

So instead I'm just going to write a wrapper around libuv and with this
pattern in mind. It should also be interesting to see what sorts of IO
throughput we can get using libuv instead of Erlang's native async
thread pool.

And... we're off.

[file_handling]: https://github.com/erlang/otp/blob/maint/erts/preloaded/src/prim_file.erl#L1032-1044

Fri 7/6 7:36 PM
---------------

First step is to fill out the project structure as well as pull libuv down
and get it building. I'll just mimic what DizzyD does for eleveldb now.

Bleeding edge deps are a love/hate relationship.

Fri 7/6 7:55 PM
---------------

Initial project structure complete. The NIF builds and can be called form
Erlang. Libuv is downloaded and built statically.

Next step is to put together a bit more boiler plate code for the NIF
initialization and then I'll start working on building the general
structure for the event loop.

I've been thinking about this over the last few days and the way I'm going
to structure this NIF will be to have a configurable number of loops (which
correspond to a thread apiece) that can be specified when a file or socket
is created. This obviously means that our first order of business will be
to write the threading code around those loops with a queue to push
requests over to the event loop.
