# libNavigation

A general purpose library for navigational calculations (for ocean going vessels)


## Getting Started

This project is a Haskell library, formatted using stack.

Make sure you have git and stack installed.

Clone this repo: `git clone https://github.com/ScottSedgwick/libNavigation.git`

Build the project: `stack build`

Test the project: `stack test`


### TODO

This project is in the early stages of development - the only function it has right now is a dead reckoning calculation, 
based on Rhumb Lines, and limited to fairly short legs as it uses the mean latitude, instead of the mid latitude, for
distance correction.  Do not use for legs longer than a few hundred nautical miles.

Intended additions:
* Better dead reckoning calculations
* Great circle calculation
* A full suite of Astro-Navigation functions, including:
  * Noon sun sights
  * Running sun sights
  * Celestial shot planning functions
  * Celestial sight calculation


### Prerequisites

Git, to clone the repository: https://git-scm.com

Stack, to fetch dependencies, build and test the project: https://docs.haskellstack.org/en/stable/README/


### Installing

The easiest way to use this library in your Haskell project is if you are using Stack.

Add "libNavigation" to your "build-depends" section in your project's cabal file.

TODO: Put instructions here on adding it to the stack.yaml file.


## Running the tests

`stack test`


## Authors

* **Scott Sedgwick** - *Initial work* - [ScottSedgwick](https://github.com/ScottSedgwick/)


## License

This project is licensed under the BSD3 License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* Hat tip to [Astro Navigation Demystified](astronavigationdemystified.com) for worked examples to use in unit tests.
