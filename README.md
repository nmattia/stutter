---
sc_check-sections:
    - examples
---

# Stutter

[![Build Status](https://travis-ci.org/nmattia/stutter.svg?branch=master)](https://travis-ci.org/nmattia/stutter)

Stutter is a string utterer.

> utterer: someone who expresses in language; someone who talks (especially
> someone who delivers a public speech or someone especially garrulous)
> (www.vocabulary.com)

Stutter takes a string definition and crafts as many different strings as it
can. See the [examples](#examples) section below for inspiration.

# Installing

## Download

You can download the latest release build from the [release
page](https://github.com/nmattia/stutter/releases). The executable depends on
the `gmp` library (needed by the Haskell runtime system), which is most likely
already present on your system. If not, install it from your favorite package
manager. For Ubuntu:

``` shell
$ sudo apt-get install libgmp-dev
```

Make sure `stutter` is on your `PATH`.

## Nix

If you have [nix](http://nixos.org/nix/) installed, you can install `stutter`
with the following command:

``` shell
$ nix-env -i stutter
```

## Building

The recommended way is to build `stutter` with
[stack](https://docs.haskellstack.org/en/stable/README/). Run the following
command in the cloned repo:

``` shell
$ stack build
```

You can then install it with

``` shell
$ stack install
```

# Contributing

There are several ways you can contribute:

* Complain: Just [open an issue](https://github.com/nmattia/stutter/issues/new)
  and let me know what could be improved.
* Share a use-case: You found a cool case? Great! [open an
  issue](https://github.com/nmattia/stutter/issues/new) or (even better) a PR
  with your issue added to the [examples](#examples) below.
* Support: Share `stutter` with your friends, you never know who might need it.
* Implement: All PRs are welcome.

# Examples

Stutter can be used as a very simple `echo` clone:

``` shell
$ stutter 'Hello, World!'
Hello, World!
```

But stutter also knows how to enumerate:

``` shell
$ stutter 'foo|bar|baz'
foo
bar
baz
```

You can easily specify which parts you want to enumerate, and which parts
should always be there:

``` shell
$ stutter 'My name is (what\?|who\?|Slim Shady)'
My name is what?
My name is who?
My name is Slim Shady
```

Stutter can also enumerate file contents:

``` shell
$ stutter 'foo|bar|baz' > test.txt
$ stutter '(@test.txt) -- stutter was here'
foo -- stutter was here
bar -- stutter was here
baz -- stutter was here
```

And read from `stdin`:

``` shell
$ cat test.txt | stutter 'Check this out, paste: @-'
Check this out, paste: foo
Check this out, paste: bar
Check this out, paste: baz
```

Stutter also likes ranges:

``` shell
$ stutter '[0-9a-f]'
0
1
2
3
4
5
6
7
8
9
a
b
c
d
e
f
```

Of course, it can all be used together:
``` shell
$ stutter 'My name is (@test.txt) [a-c] (who\?|what\?|Slim Shady)'
My name is foo a who?
My name is foo a what?
My name is foo a Slim Shady
My name is foo b who?
My name is foo b what?
...
My name is baz c who?
My name is baz c what?
My name is baz c Slim Shady
```

Stutter can teach you binary:

``` shell
$ stutter '(0b(0|1){#|5})|I know binary!'
0b00000
0b00001
0b00010
0b00011
0b00100
0b00101
...
0b11010
0b11011
0b11100
0b11101
0b11110
0b11111
I know binary!
```

Stutter can repeat a char:

``` shell
$ stutter 'a{42}'
a
a
a
...
$ stutter 'a{42}' | wc -l
42
```

# Release checklist


1. Make sure you're on (latest) master.

1. Bump the version in `stutter.cabal`: `0.MAJOR.MINOR.PATCH`.

> Given a version number MAJOR.MINOR.PATCH, increment the:
>
> MAJOR version when you make incompatible API changes,
> MINOR version when you add functionality in a backwards-compatible manner, and
> PATCH version when you make backwards-compatible bug fixes.

1. Commit the updated `stutter.cabal` file with commit name `Release
   v0.MAJOR.MINOR.PATCH`.
1. Tag the commit with `git tag v0.MAJOR.MINOR.PATCH`.
1. Push with `git push --follow-tags`.
1. Run `stack update --pvp-bounds both .` to upload `stutter` to `hackage`.
