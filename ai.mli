(* [determine_move state] is the only function that is open to the other
   interfaces. This function will take in a state and return a command that the
   player should take. The command is determined using an algorithm that
   determines the state which is best for the current player
   which means it is the worst for all the
   other players.
*)
val determine_move : State.state -> Command.command
