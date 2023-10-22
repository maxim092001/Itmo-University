pi{TeamName}(
	Teams
	nj (
		pi{TeamId}(Teams)
		except
		pi{TeamId}(
			select{Accepted = 1}(
					Sessions
					nj Runs
				)
			)
		)
)