# GHCJS-LD

## Introduction

This project implements a linker for GHCJS-style `.js_o` object files. Previously, this behaviour was achieved through invoking the GHCJS compiler. Now, we instead provide this stand-alone executable to link these object files so that GHC proper may absorb GHCJS as a cross-compilation target, allowing Cabal, etc, to invoke this in place of `ld`.

## Installation

Installation through Cabal:
```
cabal install ghcjs-ld
```

## Usage

```
ghcjs-ld [options-and-inputs]
```

Options:
```
-v/--verbose: print linking logging/warnings
-o/--output: set the output file (default "link-output.js")
--debug/--prof: reduce renaming of output definition names
--prof: setup the output code for profiling
```

## Cabal's Usage

