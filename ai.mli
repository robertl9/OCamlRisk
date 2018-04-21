(* [determineMove state action] is the only function that is open to the other
   interfaces. This function will take in a state and return an action that the current
   player should take. The action will be determined using an algorithm that determines the
   state which is best for the current player which means it is the worst for all the
   other players.
*)
val determineMove : state -> action
