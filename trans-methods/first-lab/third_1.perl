$start_end_empty_rows = 1;
$is_empty_row = 0;

while (<>) {
	if (/\S/) {
		$start_end_empty_rows = 0;
	}
	if ($start_end_empty_rows == 0) {
		s/^(\s+)//g;
		s/(\s+)$//g;
		s/(\s+)/ /g;
		if (/^\s*$/) {
			$is_empty_row = 1;
		} else {
			if ($is_empty_row == 1) {
				print "\n";
			}
			$is_empty_row = 0;
			print;
			print "\n";	
		}
	}
}
