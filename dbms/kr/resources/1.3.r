pi{RunId, SessionId, Letter, SubmitTime}(
    select{ContestId = :ContestId && Accepted = 1}(
       Sessions nj Runs
    )
)