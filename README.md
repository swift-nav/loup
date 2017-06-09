[![loup][loup-img]][loup]

# [loup][loup]

[![Package version][hackage-img]][hackage]
[![Build status][travis-img]][travis]
[![Dependency status][deps-img]][deps]

Loup is a wrapper around Amazon Simple Workflow Service providing work pools: it
providers a decider that implements work pool plans, an actor that runs
commands, and a converger that manages workers. See [examples](examples).


## Development

`loup` has a shakefile/makefile to provide convience around building and testing:

    # build the project's libraries, executables, and tests
    $ ./Shakefile.hs build-tests-error
    
    # test the project
    $ ./Shakefile.hs tests-error
    
    # start an interpreter with the project's libraries, executables, and tests loaded
    $ ./Shakefile.hs ghci-tests
    
    # install the project's executables
    $ ./Shakefile.hs install
    
    # clean the project
    $ ./Shakefile.hs clean
    
    # lint the project source code
    $ ./Shakefile.hs lint
    
    # format the project source code
    $ ./Shakefile.hs format


## Dependencies

To build, install, run, and test `loup`, the following dependencies may be required:

+ [stack][stack]


[loup]:        https://github.com/swift-nav/loup
[loup-img]:    https://cloud.githubusercontent.com/assets/60851/8178609/a077a326-13c4-11e5-9d54-3e417fc6dd6c.jpg
[hackage]:     https://hackage.haskell.org/package/loup
[hackage-img]: https://img.shields.io/hackage/v/loup.svg?style=flat
[travis]:      https://travis-ci.org/swift-nav/loup
[travis-img]:  https://img.shields.io/travis/swift-nav/loup/master.svg?style=flat
[deps]:        http://packdeps.haskellers.com/feed?needle=loup
[deps-img]:    https://img.shields.io/hackage-deps/v/loup.svg?style=flat
[stack]:       https://docs.haskellstack.org/en/stable/README/#how-to-install
