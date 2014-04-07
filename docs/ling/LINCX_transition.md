latex input:            mmd-article-header
Title:			Making LINCX the primary LINC version
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			05/04/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Summary

* The LINC version based on LINCX should be transferred to LINC-Switch repo by
March 1, 2014.

* The new capable/logical switch architecture should be postponed to ensure
continuity.

# Overview

LINCX is a version of the LINC switch with a completely new implementation
of the fast path -- performance-critical portions of the switch. LINCX shows
performance orders of magnitude better than the old version. LINCX runs on the
new Erlang platform (LING).

Currently there two versions of the LINC switch: the "Old LINC" that resides in
the [LINC-Switch](https://github.com/Flowforwarding/LINC-switch) repo and LINCX
kept in the [lincx](https://github.com/FlowForwarding/lincx) repo. The decision
has been made to merge these two versions. The new merged version -- the "New
LINC" -- should be based mostly on the LINCX codebase.

The present plan discusses the transition from the Old LINC/LINCX to the New
LINC.

# The status of LINCX

The LINCX version started as a fork of the Old LINC at the end of 2013. The Old
LINC code was reused whenever this did not compromise the performance. The
implemenation of functions that belong to the fast path are not yet complete.
Notably, the following functions are still lacking:

* Counters
* Queues
* Meters

The most performance-sensitive actions, such as Set-Field, has been completely
rewritten. Some packet-modifying actions, such as Push/Pop PBB, still use the
old (slow) code.

The build/test/deploy harness should be revisited to account for the new target
platform (LING).

In addition to these incremental changes there are bigger architectural issues
discussed below.

# The architecture must change

The Old LINC acts as an OpenFlow Capable Switch managing several logical
switches.

An allegedly better architecture is to run a single logical switch per Xen
domain and have a separate coordination entity -- the capable switch -- in
Domain 0. Such architecture should ensure precise allocation of resources
between logical switches and avoid certain bottlenecks in the Linux kernel.

The new architecture will require a new build and testing framework. The switch
software will need to talk directly to the Xen toolstack for spawning and monitoring
of logical switches.

The architecture should allow applying back pressure to remote queues, passing
control over ports to other logical switches, etc. The right mechanism for this
kind of interactions is still to be discovered.

The new architecture will obsolete most of the high-level management code of the
LINC switch and thus should be postponed until after the transition to the New
LINC. There should always be a working, compatible, and performant version
available for download.

A related forward-looking activity is the development of the next version of the
OpenFlow specification. The OpenFlow 2.0 can be explicitely based on Erlang and
Xen.

# The transition timeline

1. Prepare the first version of the New LINC (port any relevant changes, remove
stale dependencies, update tutorials, deployment scripts)

2. Replace the version in the LINC-Switch repo -- *March 1, 2014*

3. Continue development according to the prioritized task list

# Task summary -- The shopping card

The [task summary][scard] contains the initial list of tasks that can be
undertaken in the course of the transition and thereafter. The list is
maintained as the part of the repository. See docs/TASKS.md.

Priority | Description | Effort
---------|-------------|-------
A | LINCX appliance for deployment to Atom black boxes | 1wk
A | Remove stale dependencies, update Makefile | 2wk
B | Prototype the fast version of queues | 1wk
B | Port the Old LINC changes to LINCX | 1wk
B | Implement remaining packet-modifying actions | 1wk
B | Develop a strategy for exhaustive compatibility testing | 1wk
B | Create a testing enviroment using hw NICs | 1wk
B | Test the switch compatibility using Luxoft Twister | 2wk
B | Create a bug-tracking system for the New LINC | 1wk
B | Prototype counters using processes and BIFs | 1wk
B | Prototype the fast version of meters | 1wk
C | Envision and create the continuous integration environment | 3wk
C | Develop a new capable/logical switch architecture | 2wk
[The shopping card][scard]

# Appendix: the new source tree

The high-level changes to the source tree related to the transition are
summarized below.

Application | Note
------------|------------
linc | see below
linc\_us3 | drop
linc\_us4 | drop
linc\_max | see below
[apps/][apps]

File | SLOC | Note
-----|-----:|----
linc\_logic.erl | 500 | 30% rework
linc\_ofconfig.erl | 1418 | 20% rework
(other files) | (small) |
[apps/linc/][linc]

File | SLOC | Note
-----|-----:|-----
linc\_max\_convert.erl  |  142 | old, drop
linc\_max\_demo.erl  |  165 | new
linc\_max.erl  |  458 | new
linc\_max\_fast\_actions.erl  |  521 | new
linc\_max\_fast\_actions\_tests.erl  |  330 | new
linc\_max\_fast\_path.erl  |  106 | new
linc\_max\_flow.erl  |  1325 | old, drop
linc\_max\_generator.erl  |  652 | new
linc\_max\_generator\_tests.erl  |  358 | new
linc\_max\_meter\_sup.erl  |  57 | old
linc\_max\_packet.erl  |  311 | old, drop
linc\_max\_port.erl  |  302 | old, drop
linc\_max\_port\_native.erl  |  38 | old, drop
linc\_max\_preparser.erl  |  803 | new
linc\_max\_preparser\_tests.erl  |  268 | new
linc\_max\_splicer.erl  |  469 | new
linc\_max\_splicer\_tests.erl  |  217 | new
linc\_max\_sup.erl  |  63 | old, drop
[apps/linc\_max][linc_max]

Application | Note
------------|-----
eenum | TBD
enetconf | TBD
epcap | drop
lager | in use
ling\_builder | drop, use openling
meck | drop, bloats images
of\_config | in use
of\_protocol | in use
pkt | keep and avoid
procket | drop
sync | drop, not applicable
tunctl | drop
[deps/][deps]

