open Team
open Definitions
open Constants

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "attacker"

let _ = Random.self_init ()

let modular_comp mon1 mon2 =
  let (atk, spa, def, spd, spe) = (10,10,1,1,100) in 
  let score1 = (mon1.attack * atk) + (mon1.spl_attack * spa) + (mon1.defense * def) + 
		 (mon1.spl_defense * spd) + (mon1.speed * spe) in
  let score2 = (mon2.attack * atk) + (mon2.spl_attack * spa) + (mon2.defense * def) + 
		 (mon2.spl_defense * spd) + (mon2.speed * spe) in
let weighted_score mon = 
  let (atk, spa, def, spd, spe) = (1,1,1,1,1) in 
  (mon.attack * atk) + (mon.spl_attack * spa) + (mon.defense * def) + 
		 (mon.spl_defense * spd) + (mon.speed * spe) 

let modular_comp mon1 mon2 =
  let (score1, score2) = (weighted_score mon1, weighted_score mon2) in
  if score1 = score2 then 
    if mon1.cost = mon2.cost then 0
    else if mon1.cost < mon2.cost then 1
    else -1
  else if score1 > score2 then 1
  else -1

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

let creds = ref Constants.cSTEAMMON_CREDITS

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
         | h::t -> if can_purchase h credits then
		      PickSteammon(h.species)
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
	   find_available_move sorted
	| _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
    | PickInventoryRequest (gr) -> PickInventory(
				       [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
