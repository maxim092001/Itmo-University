pi{TeamName}(
	Teams nj pi{TeamId}(
        pi{TeamId}(
            Teams
        )
        cj pi{ContestId}(
            Contests
        )
        except pi{TeamId, ContestId}(
            select{Accepted = 1} (
                Runs nj Sessions
            )
        )
    )
)