pi{TeamName}(
    pi{TeamName, TeamId, ContestId}(
        Sessions nj Teams
    )
    except
    pi{TeamName, TeamId, ContestId}(
        select{Accepted = 1}(
            Sessions nj Runs nj Teams
        )
    )
)