# GHC Make Dependencies

A GHC frontend plugin for generating module dependency graphs in Makefile and JSON formats.

## Overview

This repository provides a GHC frontend plugin that can be used to obtain a module dependency graph from GHC with a simple command line invocation. The module graph is written in both the standard Makefile format and a JSON format.

This repository contains the code from [GHC merge request #11994](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11994) as an out-of-tree Cabal package.

## Features

- Generate module dependency graphs directly from GHC
- Output in standard Makefile format for build system integration
- Output in JSON format for programmatic processing
- Simple command-line interface
- Works as a GHC frontend plugin

## Usage

This code is meant to be used as part of a build-system.

**Building:**

```bash
OUTDIR=$(mktemp -d)
runhaskell Setup.hs configure --prefix $OUTDIR --package-db $OUTDIR/package.conf
runhaskell Setup.hs build
runhaskell Setup.hs install
```

**Generating a module dependency graph:**

Call GHC with MakeDeps as a frontend plugin. E.g.

```bash
ghc -package-db $OUTDIR/package.conf.d --frontend MakeDeps src/**/*.hs
```

This will create the files `deps.json` and `deps.make` in the current directory.

## Example Output

**Makefile format:**

This is just like `ghc -M`.

```makefile
λ head deps.make
# DO NOT DELETE: Beginning of Haskell dependencies
src/Distribution/Compat/Binary.o : src/Distribution/Compat/Binary.hs
src/Distribution/Compat/Exception.o : src/Distribution/Compat/Exception.hs
src/Distribution/Compat/MonadFail.o : src/Distribution/Compat/MonadFail.hs
src/Distribution/Compat/Newtype.o : src/Distribution/Compat/Newtype.hs
src/Distribution/PackageDescription/Utils.o : src/Distribution/PackageDescription/Utils.hs
src/Distribution/Utils/Base62.o : src/Distribution/Utils/Base62.hs
src/Distribution/Utils/MD5.o : src/Distribution/Utils/MD5.hs
src/Distribution/Utils/String.o : src/Distribution/Utils/String.hs
src/Distribution/Utils/Structured.o : src/Distribution/Utils/Structured.hs
```

**JSON format:**

```json
{
  "Distribution.Backpack": {
    "boot": null,
    "sources": [
      "src/Distribution/Backpack.hs"
    ],
    "modules": [
      "Distribution.Compat.CharParsing",
      "Distribution.Compat.Prelude",
      "Distribution.ModuleName",
      "Distribution.Parsec",
      "Distribution.Pretty",
      "Distribution.Types.ComponentId",
      "Distribution.Types.Module",
      "Distribution.Types.UnitId",
      "Distribution.Utils.Base62"
    ],
    "modules-boot": [],
    "packages": [],
    "cpp": [],
    "options": [],
    "preprocessor": null
  },
  "Distribution.CabalSpecVersion": {
    "boot": null,
    "sources": [
      "src/Distribution/CabalSpecVersion.hs"
    ],
    "modules": [
      "Distribution.Compat.Prelude"
    ],
    "modules-boot": [],
    "packages": [],
    "cpp": [],
    "options": [],
    "preprocessor": null
  }
}
```

> [!CAUTION]
> The json output format is not stable and it is subject to change.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This project follows the same license as GHC: BSD-3-Clause.

## Related Links

- [GHC issue](https://gitlab.haskell.org/ghc/ghc/-/issues/24384)
- [Original GHC merge request](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11994)
- [GHC Documentation](https://ghc.gitlab.haskell.org/ghc/doc/)

## Authors

- Cheng Shao (@terrorjack) for the original code
- Andrea Bedini (@andreabedini) for repackaging it as a GHC frontend plugin
 