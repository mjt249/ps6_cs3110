open Definitions
open Util
open Constants
open Netgraphics

(* You have to implement this. Change it from int to yout own state type*)
type game = State.state

let game_datafication g =
	failwith 
		"This is my grandson. He’s been your rival since you were a baby. 
		…Erm, what is his name again?"

                        (* Professor Oak, Steammon researcher extraordinaire *)
	
let game_from_data game_data = 
	failwith 
    "I like shorts! They're comfy and easy to wear!"

                        (* Youngster, upon challenging a stranger to battle *)

let handle_step g ra ba =
  match (ra, ba) with 
  | (Action red_action, Action blue_action) ->
      let red_request = match red_action with
      | SendTeamName red_name -> send_update 
      | _ -> () in
      let blue_request = match blue_action with
      | _ -> () in
      (None, (game_datafication game), red_request, blue_request)
      
  (*Ignore any other command.*)
  | _ -> (None, (game_datafication game), None, None)

let init_game () =
  (* Loading moves list and Steammon list *)
  Initialization.init_pool "moves.csv" "steammon.csv";
  let mvs = hash_to_list Initialization.move_table in
  let mons = hash_to_list Initialization.mon_table in
  let init_state = State.initial_state () in
  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)
