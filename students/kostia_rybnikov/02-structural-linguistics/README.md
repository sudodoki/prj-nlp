# Week 02 homework

## 1-sling

See [[1-sling]]

## 2-headlines

For **1. Formating** do:

```
sudo apt install librocksdb-dev  # or "brew install rocksdb" on macOS
cd headlines-formatting
stack build
# call someone you love while it builds...
stack exec headlines-formatting
```

Code is located in `headlines-formatting/src`, start from `Main.hs`

Example:

```
$ stack exec headlines-formatting
Number of titles stated correctly: 457
Corrected headlines were written into examiner-headlines-corrected.txt
```

See [[examiner-headlines-corrected.txt]] for my results.

For **2. Catch catchy headlines** do:

```
sudo apt install librocksdb-dev  # or "brew install rocksdb" on macOS
cd catch-catchy
stack build
stack exec catch-catchy -- /path/to/standford-corenlp /path/to/sentiwordnet.txt /path/to/examiner-headlines.txt /path/to/cache/file ./catchy.txt
```

It needs to be able to launch standford corenlp, so it's a bit more
fragile. Please ping me in case you have any issues! Code is in
`src/Main.hs` per usual.

`/path/to/cache/file` is a path to a directory which will be created
if not present, it'll contain a RocksDB database which will serve as a cache for CoreNLP.

Example which worked on my machine:

```
$ stack build && stack exec catch-catchy -- ~/workspace/standford-corenlp/stanford-corenlp-full-2018-02-27/ ~/Downloads/SentiWordNet_3.0.0/SentiWordNet_3.0.0_20130122.txt /home/kb/workspace/prj-nlp/tasks/02-structural-linguistics/examiner-headlines.txt /tmp/cache.rocksdb ./output.txt
```

See [[catchy.txt]] for my results.

## 3-collocations

1. продовжте синонімний ряд дієслів: "say", "tell", "speak", "claim", "communicate"

"say", "tell", "speak", "claim", "communicate", "yell", "describe", "express", "advice", "announce", "notify"

```
$ sudo apt install librocksdb-dev  # or "brew install rocksdb" on macOS
$ cd collations
$ stack build
$ stack exec collocations -- ~/workspace/standford-corenlp/stanford-corenlp-full-2018-02-27 /tmp/cache.rocksdb ../../../../tasks/02-structural-linguistics/blog2008.txt
advice: []
announce: [("recently",11),("officially",9),("publicly",7),("previously",3),("shortly",3),("quickly",3),("openly",3),("early",2),("proudly",2),("reportedly",2)]
tell: [("only",10),("really",8),("probably",7),("simply",6),("basically",5),("recently",5),("actually",4),("privately",4),("specifically",3),("essentially",3)]
speak: [("directly",23),("only",13),("publicly",10),("generally",5),("anonymously",5),("figuratively",4),("specifically",4),("openly",4),("freely",4),("Broadly",3)]
yell: [("only",1),("briefly",1),("allegedly",1),("vociferously",1),("simultaneously",1),("O'Reilly",1)]
claim: [("falsely",46),("previously",8),("Falsely",8),("repeatedly",8),("only",4),("actually",3),("O'Reilly",3),("initially",3),("credibly",3),("recently",3)]
express: [("repeatedly",6),("only",4),("actually",3),("privately",3),("freely",3),("starkly",2),("openly",2),("finally",2),("publicly",1),("politically",1)]
communicate: [("directly",3),("effectively",3),("Really",1),("broadly",1),("regularly",1),("apparently",1),("quickly",1),("hopefully",1),("actually",1),("profoundly",1)]
describe: [("only",8),("accurately",8),("really",4),("previously",3),("thusly",3),("adequately",3),("blandly",2),("Specifically",2),("actually",2),("correctly",2)]
say: [("simply",32),("actually",30),("really",29),("only",26),("explicitly",18),("publicly",15),("basically",13),("privately",12),("clearly",9),("correctly",8)]
notify: [("properly",1)]
```

Yell O'Reilly, hmm, I love it!

P.S.: I probably did pretty bad at all the manual tasks (not sure
about the programmatic ones), but I want to compensate that by getting
some bonus points for writing two libraries as I went:

- one which wraps CoreNLP into an executable, adding a RocksDB caching
  layer and ability to launch multiple concurrent workers
  https://github.com/k-bx/corenlp-parser
- one which parses SentiWordNet
  https://github.com/k-bx/sentiwordnet-parser (this one is way
  simpler, of course)
