# LINCX continuous integration

LINCX Github repo attached to [Travis CI](https://travis-ci.org) to 
trigger CI sequence on every commit. Full 
[Ryu](https://github.com/osrg/ryu) OpenFlow test suite and LINCX EUnit 
suite are executed. 

### Results

CI results can be viewed here - 
https://travis-ci.org/cloudozer/lincx/builds.
Summary of each CI run printed at end of log and for Ryu test looks 
like:

```
OK(934) / ERROR(57)
```

for EUnit tests:

```
 All 766 tests passed.
```

### TODO

Throw away travis-ci.org and implement CI through ling_builder 
extension, something like
```
./rebar lincx-ci
... wait ~12 minutes ...
... view test results ...
```

This will allow test LINCX without pushing to github and view results 
right in the terminal.

