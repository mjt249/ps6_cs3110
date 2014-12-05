open Team
open Definitions
open Constants
open Util

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "smartbot"

let _ = Random.self_init ()

let effective_damage mon (mv:move) = 
  let stab = 
    match mon.first_type, mon.second_type with
    | None, Some typ when typ = mv.element -> cSTAB_BONUS
    | Some typ, None when typ = mv.element -> cSTAB_BONUS
    | Some typ1, Some typ2 when typ1 = mv.element || typ2 = mv.element -> cSTAB_BONUS 
    | _ -> 1. in
  if is_special mv.element then
    if mv.accuracy > 70 then
      int_of_float(float_of_int(mon.spl_attack * mv.power) *. stab)
    else int_of_float(float_of_int(mon.spl_attack * mv.power * mv.accuracy / 100) *. stab)
  else if mv.accuracy > 70 then
    int_of_float(float_of_int(mon.attack * mv.power) *. stab)
  else int_of_float(float_of_int(mon.spl_attack * mv.power * mv.accuracy / 100) *. stab)

let best_move_eff_damage mon = 
  max (max (effective_damage mon mon.first_move) (effective_damage mon mon.second_move))
      (max (effective_damage mon mon.third_move) (effective_damage mon mon.fourth_move))

let weighted_score mon = 
  let (atk, spa, def, spd, spe, hp, best_move) = (1,1,1,1,1,1,1) in 
  (mon.attack * atk) + (mon.spl_attack * spa) + (mon.defense * def) + 
    (mon.spl_defense * spd) + (mon.speed * spe) + (mon.max_hp * hp) +
    ((best_move_eff_damage mon) * best_move)

let modular_comp mon1 mon2 =
  let (score1, score2) = (weighted_score mon1, weighted_score mon2) in
  if score1 = score2 then 
    if mon1.cost = mon2.cost then 0
    else if mon1.cost < mon2.cost then 1
    else -1
  else if score1 > score2 then 1
  else -1

let max_eff eff1 eff2 = 
  match eff1, eff2 with
  | Ineffective, _ -> eff2
  | _, Ineffective -> eff1
  | SuperEffective, _ -> eff1
  | _, SuperEffective -> eff2
  | NotVeryEffective, _ -> eff2
  | _, NotVeryEffective -> eff1
  | Regular, _ -> eff2

let is_ineffective att_mon def_mon = 
  let (eff1,_) = calculate_type_matchup att_mon.first_move.element 
					(def_mon.first_type, def_mon.second_type) in  
  let (eff2,_) = calculate_type_matchup att_mon.second_move.element 
					(def_mon.first_type, def_mon.second_type) in 
  let (eff3,_) = calculate_type_matchup att_mon.third_move.element 
					(def_mon.first_type, def_mon.second_type) in 
  let (eff4,_) = calculate_type_matchup att_mon.fourth_move.element 
					(def_mon.first_type, def_mon.second_type) in 
  max_eff (max_eff eff1 eff2 ) (max_eff eff3 eff4 )
 

(*compares first by attack, then spl_attack, then cost *)
let comp_by_atk mon1 mon2 =
  if mon1.attack = mon2.attack then
    if mon1.spl_attack = mon2.spl_attack then
      if mon1.cost = mon2.cost then 0
      else if mon1.cost > mon2.cost then 1
      else -1
    else if mon1.spl_attack > mon2.spl_attack then 1
    else 0
  else if mon1.attack > mon2.attack then 1
  else -1

(* compares first by power, then by accuracy *)
let comp_by_power mv1 mv2 =
  if mv1.power = mv2.power then
    if mv1.accuracy = mv2.accuracy then 0
    else if mv1.accuracy > mv2.accuracy then 1
    else 0
  else if mv1.power > mv2.power then 1
  else -1

let total_score = ref 0
let num_dead = ref 0

let can_purchase mon cred = 
  mon.cost <= cred

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)
    | StarterRequest(gs)->
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let sorted = List.rev(List.fast_sort modular_comp mons) in
	let pick = 
          try List.find(fun x -> x.curr_hp > 0) sorted 
          with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, gs, _, sp) ->
       let (a1,b1) = gs in
       let my_team = if c = Red then a1 else b1 in
       let (mons, pack, credits) = my_team in
       let sorted = List.rev(List.fast_sort modular_comp sp) in
       let rec pick_mon lst = 
	 match lst with
	 | h::[] -> PickSteammon(h.species) (* out of/low on credits *)
         | h::t -> if can_purchase h credits then(
		     (total_score := !total_score + weighted_score h);
		     PickSteammon(h.species))
		   else pick_mon t
         | [] -> failwith "no steammon to pick!" in
       pick_mon sorted
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        (match mons with
        | h::t ->
	   let mv_lst = [h.first_move;h.second_move;h.third_move;h.fourth_move] in 
	   let sorted = List.rev(List.fast_sort comp_by_power mv_lst) in
	   let rec find_available_move (lst: move list) = 
	     match lst with 
	     | hd::[] -> let _ = print_endline (h.species ^ " used " ^ (hd.name)) in
			 UseMove(hd.name)
	     | hd::tl -> if hd.pp_remaining > 0 then
			   let _ = print_endline (h.species ^ " used " ^ (hd.name)) in
			   UseMove(hd.name)
			 else find_available_move tl 
	     | _ -> failwith "WHAT HAPPENED TO MY MOVES?????" in
	   (* if is_ineffective then
	     if weighted_score h >= !total_score / cNUM_PICKS then
	       (* switch out *)
	     else 
	       (* use item *) 
	   else *)
	     find_available_move sorted
	| _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
    | PickInventoryRequest (gr) -> PickInventory(
				       [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
