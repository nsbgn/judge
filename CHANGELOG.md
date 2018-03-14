Changelog for judge
===============================================================================

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning 
Policy](https://pvp.haskell.org/).

Unreleased
----------

[0.1.3.0] - 2018-03-14
----------------------

### Changed

    * Changed name and description of data files.
    * Fixed bug that caused assumptions not to be simplified.



[0.1.2.0] - 2018-01-19
----------------------

### Changed

    * Logic files in the designated data directories can now be specified 
      without extension, but must not occur in a subdirectory.

### Removed

    * Many previously exposed internal functions are now hidden.



[0.1.1.0] - 2018-01-18
----------------------

### Added

    * Tableaux are now postprocessed to remove obviously superfluous rules.
    * Tableaux now document which formulas triggered branch closure.

### Removed

    * The `negation` and `contradicts` functions have been removed. 
    
### Changed

    * Closure is now exclusively triggered on properly signed formulas --- no 
      longer on finding `[T] A` and `[T] A -> 0`, and no longer on finding 
      `[T] A` for some assumption `A`.



[0.1.0.0] - 2018-01-17
----------------------

### Added

    * First versioned release. Although it is the first version, the 
      application has been incubating for a while and should be mostly stable. 
      The commit history will take you deeper into the past.
