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

## By hand

To build this project you need a somewhat recent GHC and cabal. A simple
`$ cabal build j` should suffice. Then you can either use the compiled binary,
of which the path can be found via `cabal list-bin j` or run it via cabal like
`cabal run j -- type GetHandleId`.

## nix

If you have a working [nix/nixos](https://nixos.org/) installation you
can directly run it via `nix run github:lep/jassbot -- type GetHandleId`. Do
note though that you still have to initialize jassbot once via the `init`
subcommand.
