open Definitions
open Util
open Constants
open Netgraphics
open State

(* You have to implement this. Change it from int to yout own state type*)
module GameState = State.GameState
type game = GameState.state

let game_datafication g =
	failwith 
		"This is my grandson. He’s been your rival since you were a baby. 
		…Erm, what is his name again?"

                        (* Professor Oak, Steammon researcher extraordinaire *)
	
let game_from_data (game_data:game_status_data) : game = 
	failwith 
    "I like shorts! They're comfy and easy to wear!"

                        (* Youngster, upon challenging a stranger to battle *)

let action_handler (g:game) (c:color) (a:action) : command = 
  match a with
  | SelectStarter startermon -> DoNothing
  | PickSteammon mon -> DoNothing
  | PickInventory inv ->  DoNothing
  | SwitchSteammon mon -> DoNothing
  | UseItem (i, iname) -> DoNothing
  | UseMove move -> DoNothing 
  | SendTeamName _ -> failwith "Both team names updated already."

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  match (ra, ba) with 

  (*Initial team name response to update the GUI *)
  | (Action (SendTeamName red_name), Action (SendTeamName blue_name)) ->
      Netgraphics.send_update (InitGraphics (red_name, blue_name));
      (None, (game_datafication g), None, None)

  (* Both players respond with an action *)
  | (Action red_action, Action blue_action) ->
      let red_request = action_handler g Red red_action in
      let blue_request = action_handler g Blue blue_action in
      (None, (game_datafication g), Some red_request, Some blue_request)

  (* Only one player responded with an action *)
  | (DoNothing, Action blue_action) -> 
      (None, (game_datafication g), None, None)
  | (Action red_action, DoNothing) -> 
      (None, (game_datafication g), None, None)

  (*Ignore any other command.*)
  | _ -> (None, (game_datafication g), None, None)

let init_game () : game * request * request * move list * steammon list =
  (* Loading moves list and Steammon list *)
  Initialization.init_pool "moves.csv" "steammon.csv";
  let mvs = hash_to_list Initialization.move_table in
  let mons = hash_to_list Initialization.mon_table in

  (* Creating a blank state for the beginning of the game *)
  let init_state = GameState.initial_state () in

  (* Setting the move list and the Steammon list for the game *)
  GameState.set_move_list init_state mvs;
  GameState.set_steammon_list init_state mons;
  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)

 

(*Function that responds to SendTeamName calls draft_phase*)
let draft_phase g r b = 
  

