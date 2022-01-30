while (<>) {
	print if /(.*)z.{3}z(.*)/;
}
