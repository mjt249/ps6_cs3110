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
  let r_mons = GameState.get_steammon_list g Red in
  let r_inv = GameState.get_inv g Red in
  let r_creds = GameState.get_creds g Red in
  let r_team = (r_mons, r_inv, r_creds) in
  let b_mons = GameState.get_steammon_list g Blue in
  let b_inv = GameState.get_inv g Blue in
  let b_creds = GameState.get_creds g Blue in
  let b_team = (b_mons, b_inv, b_creds) in
  (r_team, b_team)
	
let game_from_data (game_data:game_status_data) : game = 
  let (r_team, b_team) = game_data in
  let (r_mons, r_inv, r_creds) = r_team in
  let (b_mons, b_inv, b_creds) = b_team in
  (*GameState.set_steammons !game_instance Red r_mons;*)
  GameState.set_inv !game_instance Red r_inv;
  GameState.set_creds !game_instance Red r_creds;
  (*GameState.set_steammons !game_instance Blue b_mons;*)
  GameState.set_inv !game_instance Blue b_inv;
  GameState.set_creds !game_instance Blue b_creds;
  !game_instance

let draft_phase g ra ba = failwith "Implement draft_phase"  
let stock_inventories g rc bc =
  let cost_lst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_FULLHEAL; cCOST_REVIVE; 
                  cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in
  let default_inv = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE; cNUM_FULL_HEAL; 
                     cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in
  let stock_inventory_of (c: color) (inv: inventory) = 
    let cost = List.fold_left2 (fun worth item total -> worth*item + total) 0 cost_lst inv in
    if cost > cSTEAMMON_CREDITS then 
      GameState.set_inv g c default_inv
    else
      GameState.set_inv g c inv in
  let error_wrapper (c: color) = function
    | Action (PickInventory inv) -> stock_inventory_of c inv
    | DoNothing -> stock_inventory_of c default_inv
    | _ -> failwith "Neither PickInventory nor DoNothing" in
  error_wrapper Red rc;
  error_wrapper Blue bc;
  (Some (Request (StarterRequest (game_datafication g))),
    Some (Request (StarterRequest (game_datafication g))), None) 
(* steammon if there exists at least one steammon that has not fainted,
 * None otherwise *)
let rec faint_check (lst:steammon list) : steammon option =
  match lst with
  | [] -> failwith "No steammons in list"
  | h::[] when h.curr_hp > 0 -> Some h
  | h::[] -> None
  | h::t when h.curr_hp > 0 -> Some h
  | h::t -> faint_check t

let opp_color c =
  match c with
  | Red -> Blue
  | Blue -> Red 

(*Tries to get an arbitrary_starter steammon, if all steammons have*)
(*fainted, check whether all the opposing steammons have fainted.*)
(*If so, game is a tie. If not, the opposing team wins. If there is*)
(*a steammon on the team that has not fainted, return that steammon.*)
let arbitrary_starter g c : steammon option * game_result option = 
  let steammon_list = GameState.get_steammon_list g c in
  match (faint_check steammon_list) with
  | None -> 
      (let opp_steammon_list = GameState.get_steammon_list g (opp_color c) in
      match (faint_check opp_steammon_list) with
      | None -> (None, Some Tie)
      | _ -> (None, Some (Winner (opp_color c))))
  | Some s -> (Some s, None)

let valid_steammon s reserve_pool : bool = 
  (Table.mem reserve_pool s) && 
    ((Table.find reserve_pool s).curr_hp > 0)

(*Sets the active steammon from reserve_pool if the active steammon*)
(*faints or has not been chosen yet. If all reserve steammons are 
 *fainted, a game_result is set.*)
let battle_starter g c ac : game_result option =
  let reserve_pool = GameState.get_reserve_pool g c in
  match ac with
  | SelectStarter s when (valid_steammon s reserve_pool) ->
      let mon = Table.find reserve_pool s in
      GameState.swap_active_steammon g c mon;
      None
  | _ -> 
      match (arbitrary_starter g c) with
      | (None, None) -> failwith "Starter invariant failure"
      | (None, res) -> res
      | (_, None) -> None
      | (_, _) -> failwith "Starter invariant failure 2"

let battle_phase g rc bc :
  command option * command option * game_result option = 
  (* Apply the status effects, handle the outstanding actions and
   * then send out requests depending on whose turn it is *)
  match (GameState.get_active_mon g Red) with
  | _ -> (None, None, None)

(*no more expected_action in state
let default_action (g:game) (c:color) : action = 
  GameState.get_exp g c *)

(* Given the two actions, completes the action of the player 
 * running first and then completes the second. Returns a tuple
 * of command for red palyer, command for blue player and a result 
 * Each function's output called by the action handler should match the 
 * output of the action_handler *)
let action_handler (g:game) (rc: command) (bc: command) : 
  command option * command option * game_result option = 
  let current_phase = GameState.get_phase g in
  match current_phase with
  | GameState.TeamName -> failwith "Both team names updated already."
  | GameState.Draft -> draft_phase g rc bc
  | GameState.Inventory -> stock_inventories g rc bc
  | GameState.Battle -> battle_phase g rc bc

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
      (*GameState.set_exp g Red (PickSteammon "");
      GameState.set_exp g Blue (PickSteammon "");*)
      let r_pick_req = 
        Request (PickRequest (Red, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_steammon_list g Red))) in
      let b_pick_req = 
        Request (PickRequest (Blue, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_steammon_list g Red))) in
      (None, (game_datafication g), Some r_pick_req, Some b_pick_req)

  (*commands that aren't the init*)
  | _, _ ->
      let (red_request, blue_request, result) = 
        action_handler g ra ba in
      (result, (game_datafication g), red_request, blue_request)

  (*WON'T REACH THESE CASES ANYMORE
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
      (result, (game_datafication g), red_request, blue_request) *)

let init_game () : game * request * request * move list * steammon list =
  (* Creating a blank state for the beginning of the game *)
  let init_state = !game_instance in

  (* Loading moves list and Steammon list *)
  let mvs = GameState.get_move_list init_state in
  let mons = GameState.get_base_mons init_state in

  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)
