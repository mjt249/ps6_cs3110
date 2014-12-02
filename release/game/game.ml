open Definitions
open Util
open Constants
open Netgraphics

(* You have to implement this. Change it from int to yout own state type*)
module GameState = State.GameState
type game = GameState.state

(*
 *Internally keeping track of a game instance to handle translating
 *between game_status_data and game types.
 *)
let game_instance = ref (GameState.initial_state ()) 

let game_datafication (g:game) : game_status_data =
  let r_mons = GameState.get_player_steammons g Red in
  let r_inv = GameState.get_inv g Red in
  let r_creds = GameState.get_creds g Red in
  let r_team = (r_mons, r_inv, r_creds) in
  let b_mons = GameState.get_player_steammons g Blue in
  let b_inv = GameState.get_inv g Blue in
  let b_creds = GameState.get_creds g Blue in
  let b_team = (b_mons, b_inv, b_creds) in
  (r_team, b_team)
	
let game_from_data (game_data:game_status_data) : game = 
  let (r_team, b_team) = game_data in
  let (r_mons, r_inv, r_creds) = r_team in
  let (b_mons, b_inv, b_creds) = b_team in
  GameState.set_player_steammons !game_instance Red r_mons;
  GameState.set_inv !game_instance Red r_inv;
  GameState.set_creds !game_instance Red r_creds;
  GameState.set_player_steammons !game_instance Blue b_mons;
  GameState.set_inv !game_instance Blue b_inv;
  GameState.set_creds !game_instance Blue b_creds;
  !game_instance

let draft_phase g ra ba = failwith "Implement draft_phase"
let stocking_inventory g ra ba = failwith "Implement inventory phase"
let battle_phase g ra ba = failwith "Implement battle_phase"

(* If an invalid message or a message is missing, this 
 * method performs the default expected action and returns a 
 * game_output and a game_result if exists *)
let default_action (g:game) (c:color) : action = 
  GameState.get_exp g c

(* Given the two actions, completes the action of the player 
 * running first and then completes the second. Returns a tuple
 * of command for red palyer, command for blue player and a result *)
let action_handler (g:game) (ra:action) (ba: action) : 
  command option * command option * game_result option = 
  let current_phase = GameState.get_phase g in
  match current_phase with
  | GameState.TeamName -> failwith "Both team names updated already."
  | GameState.Draft -> draft_phase g ra ba
  | GameState.Inventory -> stocking_inventory g ra ba
  | GameState.Battle -> battle_phase g ra ba

(* check if fainted, need blue counterpart, fix orderings, maybe what I should
   do is return a new list of steammon and take the head and make it the new active
   and the rest the remaining ones *)
(*
let handle_beginning_status_red (g: game) (steammon: steammon list): unit = 
  let fate = Random.int 101 in
  match steammon with
  | [] -> ()
  | hd::tl when hd.status = None -> handle_beginning_status g tl
  | hd::tl when hd.status = Some Paralyzed -> 
     ignore(if is_active_red hd then 
	      if fate <= cPARALYSIS_CHANCE then set_can_use_moves_red g false
	      else set_effective_speed_red g ((get_red_eff_speed g) / cPARALYSIS_SLOW)
	    else ())
     handle_beginning_status tl
  | hd::tl when hd.status = Some Asleep ->
     ignore(if is_active_red hd then 
	      if fate <= cWAKE_UP_CHANCE then hd.status <- (*make new copy *)
	      else set_can_use_moves_red false
	    else ())
     handle_beginning_status tl
  | hd::tl when hd.status = Some Frozen ->
     ignore(if is_active_red hd then 
	      if fate <= cDEFROST_CHANCE then hd.status <- (*make new copy *)
	      else set_can_use_moves_red false
	    else ())
     handle_beginning_status tl
  | hd::tl when hd.status = Some Confused ->
  | hd::tl when hd.status = Some Poisoned ->
  | hd::tl when hd.status = Some Burned ->     
 *)	      

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  (* Handle status effects that occur at end of turn *)
  match (ra, ba) with
  (*Initial team name response to update the GUI *)
  | (Action (SendTeamName red_name), Action (SendTeamName blue_name)) ->
      Netgraphics.send_update (InitGraphics (red_name, blue_name));
      GameState.set_exp g Red (PickSteammon "");
      GameState.set_exp g Blue (PickSteammon "");
      let r_pick_req = 
        Request (PickRequest (Red, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_steammon_list g))) in
      let b_pick_req = 
        Request (PickRequest (Blue, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_steammon_list g))) in
      (None, (game_datafication g), Some r_pick_req, Some b_pick_req)

  (* Both players respond with an action *)
  | (Action red_action, Action blue_action) ->
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), red_request, blue_request)

  (* Only one player responded with an action *)
  | (_, Action blue_action) -> 
      let red_action = default_action g Red in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), red_request, blue_request)
  | (Action red_action, _) -> 
      let blue_action = default_action g Blue in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), red_request, blue_request)

  (* Any other command should make the game run the default action
   * as defined by 4.6 in write-up *)
  | _ -> 
      let blue_action = default_action g Blue in
      let red_action = default_action g Red in
      let (red_request, blue_request, result) = 
        action_handler g red_action blue_action in
      (result, (game_datafication g), red_request, blue_request)

let init_game () : game * request * request * move list * steammon list =
  (* Loading moves list and Steammon list *)
  Initialization.init_pool "moves.csv" "steammon.csv";
  let mvs = hash_to_list Initialization.move_table in
  let mons = hash_to_list Initialization.mon_table in

  (* Creating a blank state for the beginning of the game *)
  let init_state = !game_instance in

  (* Setting the move list and the Steammon list for the game *)
  GameState.set_move_list init_state mvs;
  GameState.set_steammon_list init_state mons;
  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)
