# jassbot

Some of the functionality of our IRC bot for #inwc.de-maps @ Quakenet

# Usage

First run init via `$ j init common.j Blizzard.j`. Then one can either do a
fuzzy search by name, parameter and/or return type. The signature of any
native/function in common.j and Blizzard.j can also be queried.

Some examples:

    $ j search "takes handle returns integer"
    native GetHandleId takes handle h returns integer
    function GetHandleIdBJ takes handle h returns integer
    native BlzFrameGetTextSizeLimit takes framehandle frame returns integer

    $ j type SetTextTagText
    native SetTextTagText takes texttag t, string s, real height returns nothing

Also make sure to pass `-h` for a useful help message

# Installation

First run `make init` followed by `make`. This project uses GNUMake, GHC 8.6,
megaparsec-7 and optparse-applicative.

