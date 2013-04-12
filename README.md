weighter: user karma/point/weight management
============================================

Status
------
`weighter` is user karma system.  You can add, remove,
and transfer points from one user (or any arbitrary
object) to another.

`weighter` has a built in time decay karma setup if
you want to reward earlier users with a higher
base karma.  A user who signs up on the first day
should be rewarded with more arbitrary points
than a user signing up on the 500th day.

`weighter` has basic checks so a user can't
spend more points than they have, so they can't
transfer points from theirself to theirself,
and it has an entire system of inheriting points
from a parent user or sponsor onto a child (or
referred) account.

All user weights are stored in redis.

Usage
-----
See the tests for basic usage examples.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
        rebar eunit skip_deps=true suite=weighter

Next Steps
----------
Maybe adding a JSON API would be nice so other non-erlang
services (or non-local-redis services) can consume (or
contribute) weight information.
