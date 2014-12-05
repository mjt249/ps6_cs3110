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

let calc_multiplier (att_mon: steammon) (def_mon: steammon) (mv: move) =
  let stab = 
    match att_mon.first_type, att_mon.second_type with
      | None, Some typ when typ = mv.element -> cSTAB_BONUS
      | Some typ, None when typ = mv.element -> cSTAB_BONUS
      | Some typ1, Some typ2 when typ1 = mv.element || typ2 = mv.element -> cSTAB_BONUS 
      | _ -> 1. in
    let (eff, type_mult) = calculate_type_matchup mv.element (def_mon.first_type, 
                        def_mon.second_type) in
    let burn = if att_mon.status = Some Burned then cBURN_WEAKNESS else 1. in
    let rand = float_of_int((Random.int (101 - cMIN_DAMAGE_RANGE)) + cMIN_DAMAGE_RANGE)  /. 100. in
    (stab *. type_mult *. burn *. rand, eff) 

let find_most_damage (gr: game_status_data) (c:color): move= 
  let (a1, b1) = gr in
  let my_team = if c = Red then a1 else b1 in
  let opp_team = if c = Blue then a1 else b1 in
  let (my_mons, my_pack, my_credits) = my_team in
  let (opp_mons, opp_pack, opp_credits) = opp_team in
  let my_active_mon = List.hd(my_mons) in
  let opp_active_mon = List.hd(opp_mons) in
  let move_list = [my_active_mon.first_move; my_active_mon.second_move; 
                     my_active_mon.third_move; my_active_mon.fourth_move] in
  let get_move_mult (m: move) = 
    let (multiplier, eff) = calc_multiplier my_active_mon opp_active_mon m in
    multiplier in
  let calc_damage (m:move) = 
    match m.target with
    | User -> -1
    | Opponent ->
        let mult = get_move_mult m in
        if m.power = 0 then 0 (*non damaging *)
        else if is_special m.element then 
          if m.accuracy >= 70 then
            calculate_damage my_active_mon.spl_attack opp_active_mon.spl_defense m.power mult
          else (calculate_damage my_active_mon.spl_attack opp_active_mon.spl_defense m.power mult)*m.accuracy
        else if m.accuracy < 70 then
            calculate_damage my_active_mon.spl_attack opp_active_mon.spl_defense m.power mult
        else (calculate_damage my_active_mon.spl_attack opp_active_mon.spl_defense m.power mult)*m.accuracy in
      let damage_list = List.map calc_damage move_list in
      let damage_tagged_moves =  List.fold_left2 (fun lst damage mov -> (damage, mov)::lst) [] damage_list move_list in 
      let sorted_tagged_lst = List.sort (fun (d1, m1) (d2, m2) -> d1 - d2) damage_tagged_moves in
      let sorted_moves = List.map (fun (damage, mov) -> mov) sorted_tagged_lst in
  List.hd(sorted_moves)

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
  let (atk, spa, def, spd, spe, hp, best_move) = (1,1,1,30,1,5,10) in 
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

let empty_inv inv =
  match inv with
  | [0;0;0;0;0;0;0] -> true
  | _ -> false
 
let rec faint_check mons = 
  match mons with 
  | [] -> None
  | h::[] -> if h.curr_hp <= 0 then (Some h) else None
  | h::t -> if h.curr_hp <= 0 then (Some h) else (faint_check t)

let rec first_restore mons = 
  match mons with
  | [] -> None
  | h::[] -> if h.curr_hp <= h.max_hp then (Some h) else None
  | h::t -> if h.curr_hp <= h.max_hp then (Some h) else (first_restore t)

let rec first_heal mons =
  match mons with
  | [] -> None
  | h::[] -> if (h.status <> None) then (Some h) else None
  | h::t -> if (h.status <> None) then (Some h) else (first_heal t)

let use_item smons inv =
  let mons = List.rev(List.fast_sort modular_comp (List.tl smons)) in
  let revive_count = List.nth inv 2 in
  let fullheal_count = List.nth inv 3 in
  let max_potion_count = List.nth inv 1 in
  if (revive_count > 0) && ((faint_check mons) <> None)  then
    (match (faint_check mons) with
    | Some mon -> UseItem (Revive, mon.species)
    | None -> failwith "Shouldn't happen")
  else if (max_potion_count > 0) && ((first_restore mons) <> None) then
    (match (first_restore mons) with
    | Some mon -> UseItem (MaxPotion, mon.species)
    | None -> failwith "Shouldn't happen")
  else if (fullheal_count > 0) && ((first_heal mons) <> None) then
    (match (first_heal mons) with
    | Some mon -> UseItem (FullHeal, mon.species)
    | None -> failwith "Shouldn't happen")
  else
    failwith "Inventory invariant failure"

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
  let (eff1,_) = if att_mon.first_move.power > 0 then 
		   calculate_type_matchup att_mon.first_move.element 
					(def_mon.first_type, def_mon.second_type) 
		 else (Ineffective, 0.) in  
  let (eff2,_) = if att_mon.second_move.power > 0 then
		   calculate_type_matchup att_mon.second_move.element 
					(def_mon.first_type, def_mon.second_type) 
		 else (Ineffective, 0.) in 
  let (eff3,_) = if att_mon.third_move.power > 0 then 
		   calculate_type_matchup att_mon.third_move.element 
					(def_mon.first_type, def_mon.second_type) 
		 else (Ineffective, 0.) in 
  let (eff4,_) = if att_mon.fourth_move.power > 0 then
		   calculate_type_matchup att_mon.fourth_move.element 
					(def_mon.first_type, def_mon.second_type) 
		 else (Ineffective, 0.) in 
  (max_eff (max_eff eff1 eff2 ) (max_eff eff3 eff4 )) = Ineffective
 
(* going to be an option type *)
(* assuming active steammon is ineffective *)
let rec switch_out mon_lst opp : steammon option= 
  if cNUM_PICKS = 1 then None
  else match List.tl mon_lst with
       | [] -> None
       | hd::tl -> if is_ineffective hd opp && hd.curr_hp > 0 then
		     switch_out tl opp
		   else Some hd

let movement_is_restricted mon = 
  match mon.status with 
  | Some x when x = Paralyzed || x = Asleep || x = Frozen || x = Confused -> true
  | _ -> false

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
	let opponent_team = if c = Red then b1 else a1 in
	let (opp_mons, _, _) = opponent_team in
	let opp = List.hd opp_mons in
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
	   let compare_moves (m1: move) (mv2: move) : int =
	     let get_move_mult (m: move) =
               let (multiplier, eff) = calc_multiplier h opp m in
               multiplier in
	     let calc_damage (m:move) =
               match m.target with
               | User -> -1
               | Opponent ->
		  let mult = get_move_mult m in
		  if m.power = 0 then 0 (*non damaging *)
		  else if is_special m.element then
		    if m.accuracy >= 70 then
                      calculate_damage h.spl_attack 
				       opp.spl_defense m.power mult
          else (calculate_damage h.spl_attack 
               opp.spl_defense m.power mult)*m.accuracy
        else if m.accuracy < 70 then
                      calculate_damage h.spl_attack 
               opp.spl_defense m.power mult
        else (calculate_damage h.spl_attack 
					 opp.spl_defense m.power mult)*m.accuracy in
	     (calc_damage m1) - (calc_damage mv2) in
	   let mv_lst = [h.first_move;h.second_move;h.third_move;h.fourth_move] in 
	   let sorted = List.rev(List.fast_sort compare_moves mv_lst) in
	   let rec find_available_move (lst: move list) = 
	     match lst with 
	     | hd::[] -> let _ = print_endline (h.species ^ " used " ^ (hd.name)) in
			 UseMove(hd.name)
	     | hd::tl -> if hd.pp_remaining > 0 then
			   let _ = print_endline (h.species ^ " used " ^ (hd.name)) in
			   UseMove(hd.name)
			 else find_available_move tl 
	     | _ -> failwith "WHAT HAPPENED TO MY MOVES?????" in
	   if is_ineffective h opp then
	     if weighted_score h >= !total_score / cNUM_PICKS then
	       (* switch out *)
               match switch_out mons opp with
               | None -> find_available_move sorted
               | Some monster -> failwith "TODO"
	     else 
	       (* use item *)
	       failwith "TODO"
	   else 
	     find_available_move sorted
	| _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
    | PickInventoryRequest (gr) -> PickInventory(
				       [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
