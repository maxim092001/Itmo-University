while (<>) {
    s/(\w+)(\W+)(\w+)/$3$2$1/;
    print;
}
