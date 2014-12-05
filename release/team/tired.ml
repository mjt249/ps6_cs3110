open Team
open Definitions
open Constants
open Util

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "so tired" 

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
       (match sp with
         | h::t ->
            let length = List.length sp in
            let my_pick = List.nth sp (Random.int length) in
              PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
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
             if m.power = 0 then 
                0 (*non damaging *)
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
        UseMove ((List.hd(sorted_moves)).name)
   | PickInventoryRequest (gr) -> PickInventory(
          [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
           cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
