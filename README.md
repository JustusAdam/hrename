# hrename

Bulk rename files based on regular expressions and format strings.

Written in [Haskell](//haskell.org).

## Installation

Clone the repository.

Run `cabal install` in the (repository) directory.

## Usage

Run `hrename [options]`

    Help Options:
      -h, --help
        Show option summary.
      --help-all
        Show all help options.

    Application Options:
      -d, --directory :: text
        Directory who's content is to be renamed
        default: "."
      -e, --regex :: maybe<text>
        Regex to use for conversion (required)
        the regex matcher utilizes the
        icu standard, a current copy of which can be found here:
        http://userguide.icu-project.org/strings/regexp
      -f, --format :: maybe<text>
        Format string for conversion target (required)
      -r, --recursive :: bool
        Scan subdirectories recursively, applying the renaming (untested)
    default: false
