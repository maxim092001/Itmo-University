pi{RunId, SessionId, Letter, SubmitTime, Accepted}(
    select{ContestId = :ContestId && TeamId = :TeamId}(
       Sessions nj Runs
    )
)