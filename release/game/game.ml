open Definitions
open Util
open Constants
open Netgraphics

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

(* If an invalid message or a message is missing, this 
 * method performs the default expected action and returns a 
 * game_output and a game_result if exists *)
let default_action (g:game) (c:color) : action = 
  match c with
  | Red -> GameState.get_red_exp g
  | Blue -> GameState.get_blue_exp g

(* Given the two actions, completes the action of the player 
 * running first and then completes the second. Returns a tuple
 * of command for red palyer, command for blue player and a result *)
let action_handler (g:game) (ra:action) (ba: action) : 
  command * command * game_result option = 
  (*TODO*)
  match ra with
  | SelectStarter startermon -> (DoNothing, DoNothing, None)
  | PickSteammon mon -> (DoNothing, DoNothing, None)
  | PickInventory inv -> (DoNothing, DoNothing, None)
  | SwitchSteammon mon -> (DoNothing, DoNothing, None)
  | UseItem (i, iname) -> (DoNothing, DoNothing, None)
  | UseMove move -> (DoNothing, DoNothing, None)
  | SendTeamName _ -> failwith "Both team names updated already."

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  match (ra, ba) with 

  (*Initial team name response to update the GUI *)
  | (Action (SendTeamName red_name), Action (SendTeamName blue_name)) ->
      Netgraphics.send_update (InitGraphics (red_name, blue_name));
      (*TODO*)
      (None, (game_datafication g), None, None)

  (* Both players respond with an action *)
  | (Action red_action, Action blue_action) ->
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), Some red_request, Some blue_request)

  (* Only one player responded with an action *)
  | (_, Action blue_action) -> 
      let red_action = default_action g Red in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), Some red_request, Some blue_request)
  | (Action red_action, _) -> 
      let blue_action = default_action g Blue in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), Some red_request, Some blue_request)

  (* Any other command should make the game run the default action
   * as defined by 4.6 in write-up *)
  | _ -> 
      let blue_action = default_action g Blue in
      let red_action = default_action g Red in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), Some red_request, Some blue_request)

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
