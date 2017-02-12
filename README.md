# Stutter

Stutter is a string utterer.

> utterer: someone who expresses in language; someone who talks (especially
> someone who delivers a public speech or someone especially garrulous)
> (www.vocabulary.com)

Stutter takes a string definition and crafts as many different strings as it
can.

# Examples

Stutter can be used as a very simple `echo` clone:

``` shell
$ stutter 'Hello, World!'
Hello, World!
```

But stutter also knows how to enumerate:


``` shell
$ stutter 'foo+bar+baz'
foo
bar
baz
```

You can easily specify which parts you want to enumerate, and which parts
should always be there:

``` shell
$ stutter 'My name is (what?+who?+Slim Shady)'
My name is what?
My name is who?
My name is Slim Shady
```

Stutter can also enumerate file contents:


``` shell
$ stutter 'foo+bar+baz' > test.txt
$ stutter '(@test.txt) # stutter was here'
foo # stutter was here
bar # stutter was here
baz # stutter was here
```

And work as a `paste` replacement:

``` shell
$ stutter 'Check this out, paste: @test.txt'
Check this out, paste: foo
Check this out, paste: bar
Check this out, paste: baz
```

_note: `stdin` support is on its way_


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
$ stutter 'My name is (@test.txt) [a-c] (who?+what?+Slim Shady)'
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
