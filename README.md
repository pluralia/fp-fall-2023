# Haskell Fall 2023

## Homework 2

Deadline: 12 Oct, 23:59
Fixes: 19 Oct, 23:59

1. Create a project `hw02` using `cabal init --cabal-version=2.4 --interactiv`
    - make sure you:
        - Created Library
        - Generated a test suite
2. Modify content of the `test` folder
    - Put `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` in `MyLibTest.hs`
    - Create `TestSpec.hs` for tests
3. Modify `.cabal` such way that:
    - `test-suite hw02-test` dep on `hspec`
    - `library` and `test-suite hw02-test` have `ghc-options: -Wall`
    - `test-suite hw02-test` has `other-modules: TestsSpec`
4. Open VSCode in `hw02` (otherwise HLS can work not properly)
5. Install linter
    - `cabal update && cabal install hlint`
6. Do your homework in the `hw02` folder
    - solution is in `src/MyLib.hs`
    - tests are in `test/TestSpec.hs`
7. Check that you have no
    - warnings by the compiler
    - linter's suggestions
    - or can explain why a suggestion or a warning is not good

### Selected commands
* `cabal build`
* `cabal test`
* `hlint .` or `hlint . --report`

### Troubleshooting
- Restart HLS:
    - `<Command>` + `<Shift>` + `<P>`
    - Choose "Haskell: Restart Haskell LSP server"
- Istall package with `cabal update && cabal install <package_name>`