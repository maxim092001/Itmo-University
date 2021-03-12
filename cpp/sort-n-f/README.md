# sort command line utility

sort -- sort or merge records (lines) of text and binary files

The sort utility sorts text and binary files by lines.  A line is a record separated from the subsequent record by a newline.
A record can contain any printable or unprintable characters.  Comparisons are based on one or more sort
keys extracted from each line of input, and are performed lexicographically, according to the default (C) locale's collating rules and the
specified command-line options that can tune the actual sorting behavior.  By default, if keys are not given, sort uses entire lines for
comparison.

If no input file is specified or `-` is given instead of a file name, lines are read from standard input.

```bash
sort [OPTION] [FILE]
```

options:
* `-f, --ignore-case` - fold lower case to upper case characters, that is, perform case-independent sorting.
* `-n, --numeric-sort` - compare according to string numerical value. Strings are supposed to have optional blanks in the beginning, an optional minus sign, zero or more digits. Zero digits case should be treated as a zero value.

### Example
```bash
$ cat e.txt
784
1298
 -9
  

$ ./sort e.txt
  
1298
784
 -9

$ ./sort -n e.txt
 -9
  
784
1298
