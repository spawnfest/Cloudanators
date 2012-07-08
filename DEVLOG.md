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

Fri 7/6 8:02 PM
---------------

Beer and smoke break. Also, I need to find some better tunes.

Fri 7/6 9:44 PM
---------------

Working on getting the initial threading for event loops sorted out.
Apparently libuv depends on the Carbon framework. This is bad because if
you don't initialize CarbonCore from the main thread it segfaults. I ended
up hacking a local patch for libuv that disables this. Luckily someone had
already written a patch for IOS to avoid this so I just used that and
disabled the use of CarbonCore.

Fri 7/6 9:55 PM
---------------

The dog insists on playing. Gonna go throw a toy around for a bit.

Fri 7/6 10:06 PM
----------------

Dog tired enough for now. Back to coding.

Fri 7/6 11:14 PM
----------------

Dog wasn't quite tired enough and insisted on playing between my feet for
awhile. Eventually I bribed her with a rawhide bone.

In dev news though, I have the initial draft for starting event loops with
their queues and what not. I've verified that loading the NIF results in
having a running thread sitting in a libuv polling function.

Next up will be basic functions for file handling.

Sat 7/7 12:04 AM
----------------

After quietly reflecting while eating a piece of cold pizza and having another
smoke I've decided that the best path forward to figuring out this libuv
API is to just try and write a file:open/2 call. Also, I am very close to
taking this dog on a walk to wear her out more. She's still insiting on
me throwing the ball around.

Sat 7/7 12:27 AM
----------------

Yep. Too hard to concentrate with the dog going to town on the squeaky toy.
Headed out on a walk. Gonna try and get at least a basic open/read/write/close
API implemented before bed.

Sat 7/7 1:37 AM
---------------

Walked for 2mi. That's a bit harder when its this hot out. Took a shower
afterward and had another slice of cold pizza. Cracked another beer and I
should be good to go for another few hours. Up next is that base file API.

Sat 7/7 2:32 AM
---------------

Hit a bit of a brick wall design wise. Trying to figure out how to make this
thing simple and still mesh with expected Erlang patterns. I'm running into
some issues with matching NIF resource types with the single threaded-ness
of libuv. Lots of questions on allocation and initialization procedures
for what I assumed would be obvious resource types.

I'm starting to lean towards a single resource type that specifies what
callback functions are appropiate. Its hard to determine if that's going
to be general enough to map out to the entire libuv API or if I'm going
to paint myself into a corner.

New plan: Go watch some "How I Met Your Mother" and have a few beers and
then sleep on it. I think I'm quite close to cracking the general model for
how to organize this but I want to be sure I'm not just throwing code at
the wall to see what sticks.

Sat 7/7 2:09 PM
---------------

Definitely slept longer than I had planned on. But I'd rather lose a little
time than be trying to do this tired. Did manage to get some thinking done
on how to structure this whole libuv interaction.

The current plan is that each NIF function will create a `euv_req_t*` that
will be submitted to the specified loop. The libuv thread will then
handle the request and message pass the result back to the requesting
`ErlNifPid`. I'm a bit uncertain on the exact logic surrounding the
handle references but I think it'll work itself out.

Next step is to flesh out the request submission code and start getting to
a place where I'm creating handles to pass back to Erlang.

Sat 7/7 3:46 PM
---------------

The `euv_req_t` queuing and submission appears to be working. I've got
requests pumping into the event queue. Next up is to be able to actually
do something with those and insert them into the actual libuv event handling
system.

I'm going to start off with a basic `file_open` call which will require me
to start sending resources back to Erlang as well as being able to provide
them with new request submissions. Some of the boiler plate code is already
written but its obviously not even been smoke tested yet.

I should order some food. I'm thinking Jimmy John's.

Sat 7/7 3:51 PM
---------------

On second though I'm going to start off a bit more easily with a function
call that doesn't require the resources. One step at a time as they say.

Also, I'm ordering Jimmy John's now cause I'm hungry.

Sat 7/7 4:40 PM
---------------

Jimmy John's was delicious.

I'm staring at this request loop trying to figure out how to match up
request submissions to actual implementation code. About the only thing
that I can think of is going to end up with me maintaining a list of
ids by hand in Erlang and in C and making sure that they line up. Its
not the best solution but I don't want to insert some sort of lookup
in this tight loop if I can avoid it.

I'm going to resign myself to the corresponding lists pattern that
`prim_file` uses and just deal with it.

Sat 7/7 8:19 PM
---------------

Well I sure as shit haven't been making progress as fast as I thougth I would
be. I have managed to get a stat function returning data but for the last
hour or so I've been debugging why it appears to fail on the first call
everytime the VM starts. I'm going to write a wrapper for utime and then
we'll see what happens there.

Sat 7/7 9:05 PM
---------------

I've got a `lstat` and `utime` written and they're showing the same behavior.
I can't decide if I should just plow ahead and ignore it for now or sit down
and write a small test case for libuv.

Sat 7/7 10:03 PM
----------------

I'm a moron. I was just pulling the data out of ErlNifBinary's which isn't
null-terminated. A bit of an odd way to present in terms of the buggy
behaviour but at least it works now.

Now that I have a few basic functions working I'm going to try and setup
`euv_file:open/2` to return a file handle. The way I wrote these other
functions should allow me to handle this quite easily but I guess we'll
see if I'm crazy or not.

Sat 7/7 11:29 PM
----------------

Woot. I've got the initial code for opening and closing files written and
it appears to be working. Granted I have absolutely no idea if I have horrible
memory leaks in this code. I'm semi sure that I've got the ref counting done
properly but its complex enough I'm not 100% sure. I'll have to suss that out
more with testing when I get to that part.

Next up are the read/write functions so we can actually start moving data
in and out of this thing.

Sun 7/8 1:04 AM
---------------

RAWR reference counting. I'm having a hell of a time getting the ErlNifBinary
pattern right. Its a bit of a weird situation with these handle resources
and the docs aren't exactly clear on what I need to be freeing vs not.

I'm going to just hit it with a hammer and allocate my own buffers for the
time being so I can get to the point where this is working.

Sun 7/8 1:50 AM
---------------

Moar RAWR. Its better allocating my own buffers but I'm hitting some sort
of weird assertions in libuv itself now. Something weird is afoot and I
can't quite put my finger on it. I'm guessing I can't see something dumb
like an unitialized variable since I'm tired.

The dog is entusiastically asking to go on a walk so I'm gonna take care
of that and then come back and try and figure some more of this shit out.
