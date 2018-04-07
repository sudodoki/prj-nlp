# common-crabl

Example launch:

```
$ stack build && /usr/bin/time stack exec common-crabl -- /home/kb/Downloads/commoncrawl/CC-MAIN-20180221222354-20180222002354-00249.warc.gz
Top 10 domains:
[("com", 69573), ("ru", 7164), ("org", 6909), ("net", 5337),
 ("de", 4041), ("uk", 3264), ("jp", 2751), ("fr", 1638),
 ("pl", 1536), ("it", 1488)]
Top 10 used languages:
[(Cld2Language_ENGLISH, 19827),
 (Cld2Language_UNKNOWN_LANGUAGE, 2765),
 (Cld2Language_RUSSIAN, 2753), (Cld2Language_JAPANESE, 2467),
 (Cld2Language_GERMAN, 1937), (Cld2Language_SPANISH, 1763),
 (Cld2Language_FRENCH, 1680), (Cld2Language_PORTUGUESE, 1047),
 (Cld2Language_CHINESE, 788), (Cld2Language_ITALIAN, 747)]
90.78user 1.06system 1:31.85elapsed 99%CPU (0avgtext+0avgdata 383312maxresident)k
0inputs+0outputs (0major+480225minor)pagefaults 0swaps
```
