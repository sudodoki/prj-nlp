# conduit-xml-example

RAM is hard to keep constant if we need to gather all the teachers and
all the mistakes they've marked, but I've checked on program which
does nothing on a 5GB file and it indeed eats 11KB of memory only.

## Running

First, concat all the annotations in one big sgml:

```
cat *.sgml > concat.sgml
```

Then run like this:

```
$ stack build && stack exec extract-conll2014 -- ../conll14st-test-data/noalt/concat.sgml
100
Found teachers: 2
Found overall mistakes: 5728
Mistake annotations which intersect upon all teachers: 5066
```
