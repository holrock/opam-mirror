opam-mirror
===========

This is a tool that mirrors original upstream distribution files in OPAM
packages.  It places them into a `distfiles/` directory that follows a similar
structure to the existing `archives/` directory.

Assuming that you have an OPAM checkout in `~/git/opam-repository`, do:

    opam mirror-show-urls ~/git/opam-repository > package-list
    opam mirror-fetch-urls package-list

or

    opam mirror-fetch-urls - | opam mirror-show-urls ~git/opam-repository

