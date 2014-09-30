# LINCX continuous integration

LINCX Github repo attached to [Travis CI](https://travis-ci.org) to 
trigger CI sequence on every commit. Full 
[Ryu](https://github.com/osrg/ryu) OpenFlow test suite and LINCX EUnit 
suite are executed on dedicated box erlangonxen.org. Travis used only for commit hooks and test results viewing.

### Results

For every build Travis send email notification to commit author and to all
recipients listed in .travis.yml.

<img src="http://i.imgur.com/EfYWupE.png" width="75%"/>

More detailed results can be viewed by clicking link in above email (e.g. https://travis-ci.org/cloudozer/lincx/builds/36663202).
Summary of each CI run printed at end of log:

<img src="http://i.imgur.com/gKEkt6C.png" width="75%"/>

scroll to the end..

<img src="http://i.imgur.com/lx91H3m.png" width="75%"/>

Ryu summary designated in first red rectangle, EUnit summary in the second one.

### Possible imporovements

Generate pretty markdown table with build statistics.

### More improvements

Throw away travis-ci.org and implement CI through ling_builder 
extension, something like
```
./rebar lincx-ci
... wait ~12 minutes ...
... view test results ...
```

This will allow test LINCX without pushing to github and view results 
right in the terminal.

