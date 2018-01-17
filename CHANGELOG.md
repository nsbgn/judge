Changelog for judge
===============================================================================

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning 
Policy](https://pvp.haskell.org/).

Unreleased
----------


[0.1.1.0] - 2018-01-18
----------------------

### Added

    * Tableaux are now postprocessed to remove obviously superfluous rules.
    * Tableaux now document which formulas triggered branch closure.

### Removed

    * The `negation` and `contradicts` functions have been removed. Closure is 
      now exclusively triggered on properly signed formulas.



[0.1.0.0] - 2018-01-17
----------------------

### Added

    * First versioned release. Although it is the first version, the 
      application has been incubating for a while and should be mostly stable. 
      The commit history will take you deeper into the past.
