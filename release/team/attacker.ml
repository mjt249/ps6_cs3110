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

let creds = ref Constants.cSTEAMMON_CREDITS

let can_purchase mon = 
  mon.cost <= !creds

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)
    | StarterRequest(gs)->
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let pick = 
          try List.find(fun x -> x.curr_hp > 0) mons 
          with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
       let sorted = List.fast_sort comp_by_atk sp in
       let rec pick_mon lst = 
	 match lst with
	 | h::[] -> PickSteammon(h.species) (* out of/low on credits *)
         | h::t -> if can_purchase h then
		     ((creds := !creds - h.cost);
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
            if (h.first_move).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_move).name)) in
                UseMove((h.first_move).name)
            else if ((h.second_move).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_move).name)) in
                UseMove((h.second_move).name)
            else if ((h.third_move).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_move).name)) in
                UseMove((h.third_move).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_move).name)) in
                UseMove((h.fourth_move).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
	 | PickInventoryRequest (gr) -> PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
