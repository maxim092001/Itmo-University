while (<>) {
	print if /(^\S+$)|(^$)|(^\S+.*\S+$)/;
}
