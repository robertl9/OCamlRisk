(* [view click] is the new state affected by the actions and updates the gui as a side effect
   The state is updated using fucntions from external modules and the gui will be updated using
   external libraries.
*)
val view : click -> state
