open Constants

module GameState = struct
type state = int

type red_name = string option ref
type blue_name = string option ref

let initial_state () = 42
  
  (*Add initial Steammon Credits to Each Player by taking INITIAL_CASH from Constants*)
  
  


let get_red_name s = None
let get_blue_name s = None

(*updates Steammon_Credits. credits may be negative*)
let update_steammon_credits s team credits =
	(*add credits to Steammon_Credits*) None


end