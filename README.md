# wn-swipl
Wordnet 3 basic handling in SWI-Prolog v.7 (and up :)

##
The main contribution of these snippets can be summarized as 'how to put QLF at work'.

QLF is the SWIPL binary format of Prolog.
It matters here given the size of WN 3, indeed it speed up load times to 1/10 and more...

The WN folder is - unchanged, I think - the distribution I got from Stanford university (thanks Witzig!) in 2013, of WN-Prolog.

There is a bug (a loop) in the hypernim relation.
For instance, find about it in wn_subset.pl.

wordnet.pl source use some of mine interfaces - to display big graphs, sweeten Prolog syntax, ... so will need some work to be used,
while wn3_db.pl will be easier, still featuring the essential interfaces.

##
Enjoy!
