
### Next steps:
- [x] test the functions with more contracts 
- [x] add `exchange` valuation function
- [x] add `anytime` combinator for american options
- [x] think about what other observables could exist
- [x] implement stocks and options logic 
- [x] test the functions with invalid contracts 
- [x] custom value process print function
- [x] implement optimisation layer
- [x] implement caching
- [x] implement a contract that requires a Boolean obseravable
- [ ] benchmark caching vs graphs
- [ ] benchmark with without optimisation layer
- [ ] create graphical representation of the value process
- [x] update eval error handling
- [ ] write more quickCheck property tests 

<!-- ### Run Benchmarking 
1. Clear cabal.project.local

add
benchmarks: True

run 
cabal build 
cabal bench 
in the terminal

### Run Tests with coverage
1. Clear cabal.project.local

add
ignore-project: False
tests: True
coverage: True
library-coverage: True

run 
cabal clean
cabal configure --enable-tests --enable-coverage
cabal build --enable-coverage
cabal test --enable-coverage

### Run Main -->


## Prerequisites
- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell build tool)
- Dependencies listed in `.cabal` file

## Benchmarking
To run benchmarks:

### Step 1: Configure for benchmarks
echo "benchmarks: True" > cabal.project.local

### Step 2: Build and run benchmarks
```bash
cabal build
cabal bench caching
cabal bench optimasation-layer
```

## Running Tests with Coverage
To run tests with code coverage:

### Step 1: Configure for test coverage
```bash
echo "ignore-project: False
tests: True
coverage: True
library-coverage: True" > cabal.project.local
```


### Step 2: Clean, configure, and run tests
```bash
cabal clean
cabal configure --enable-tests --enable-coverage
cabal build --enable-coverage
cabal test --enable-coverage
```
<!-- **Note:** Coverage reports will be generated in `dist-newstyle/hpc/` (use `hpc report` to view). -->

## Running the Main Program
To build and execute the main executable:
```bash
cabal build
cabal run
```