while (<>) {
	s/(\w)(\g1+)/$1/g;
	print;
}
