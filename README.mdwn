# Gitto

For lack of a better title.

My simple utility to keep track of all the git repositories I have on
my computer(s). Also an experiment in writing scheme.

Written for `guile` 2.0.x

## Installation

Installing `gitto` is not really necessary, but it *is* more
convenient. In order to install it just run:

    # make install

This will install `gitto` to `/usr/local/`, if you would like it
somewhere else you could use the `DESTDIR` variable:

    # make install DESTDIR=/some/other/place

This will install `gitto` to `/some/other/place/`.

## Removal

If you're fed up with `gitto` and want it gone, and I do mean **now**,
you can run:

    # make uninstall

This will remove any `gitto` installed files from `/usr/local/`, if
you used the `DESTDIR` variable during installation, you should give
it the same value here, like:

    # make uninstall DESTDIR=/some/other/place

This will remove any `gitto` installed files from
`/some/other/place/`.

## Usage

Usage is, hopefully, simple, first you have to register some repos:

    $ gitto -r ~/projects/project1
    $ gitto -r ~/projects/project2
    $ gitto -r ~/projects/project3

Then you can call `gitto` without any argument to see a list of your
repos and their statuses:

    $ gitto
    project1:      0 to push, 0 to pull and is dirty
    project2:      5 to push, 2 to pull and is not dirty
    project3:      0 to push, 1 to pull and is not dirty

If you're done with one of your projects and no longer wish to track
their status you can unregister them:

    $ gitto -R ~/projects/project1


If you require further assistance:

    $ gitto -h

### `run-gitto`

As I mentioned, it is not completely necessary to install gitto, I
have added a utility to run it straight from je project directory,
although mostly for testing purposes.

Using `run-gitto` works exactly the same as using `gitto` when it is
installed.
