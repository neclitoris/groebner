# groebner

Haskell library for symbolic computation.

### Build

You will need `ghc`, `stack` and `cabal`. Recommended way to install is via [Haskell platform](https://www.haskell.org/platform/).

To build the project, use
```
stack build
```

To run tests, use
```
stack test
```

To run benchmarks, use
```
stack bench
```
Pretty benchmark report (like [this one](https://neclitoris.github.io/groebner/benchmark_report.html)) can be obtained with
```
stack bench --ba "--output report.html"
```
