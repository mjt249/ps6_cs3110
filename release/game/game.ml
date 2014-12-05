open Definitions
open Util
open Constants
open Netgraphics
open State

(* You have to implement this. Change it from int to yout own state type*)
module GameState = State.GameState
type game = GameState.state

(*
 *Internally keeping track of a game instance to handle translating
 *between game_status_data and game types.
 *)
let game_instance = ref (GameState.initial_state ()) 

let opp_color c =
  match c with
  | Red -> Blue
  | Blue -> Red 

let valid_steammon s reserve_pool : bool = 
  (Table.mem reserve_pool s) && 
    ((Table.find reserve_pool s).curr_hp > 0)

let game_datafication (g:game) : game_status_data =
  let r_mons = GameState.get_steammon_list g Red in
  let r_inv = GameState.get_inv g Red in
  let r_creds = GameState.get_creds g Red in
  let r_team = (r_mons, r_inv, r_creds) in
  let b_mons = GameState.get_steammon_list g Blue in
  let b_inv = GameState.get_inv g Blue in
  let b_creds = GameState.get_creds g Blue in
  let b_team = (b_mons, b_inv, b_creds) in
  game_instance := g;
  (r_team, b_team)

(*from team_data tuple.
  team_data: steammon list * inventory * int*)
let game_from_data (game_data:game_status_data) : game = 
  !game_instance

let team_phase g rc bc = 
  let (red_name, blue_name) = match (rc, bc) with
  (*Initial team name response to update the GUI *)
  | (Action (SendTeamName r_name), Action (SendTeamName b_name)) ->
      (r_name, b_name)
  | (_ , Action (SendTeamName b_name)) ->
      ("Red", b_name)
  | (Action (SendTeamName r_name), _) ->
      (r_name, "Blue")
  | (_,_) -> 
      ("Red", "Blue") in
  Netgraphics.send_update (InitGraphics (red_name, blue_name));
  let draft_pick = Random.int 2 in
  let r_pick_req = 
    (if draft_pick = 0 then
      Some (Request (PickRequest (Red, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_base_mons g))))
    else 
      None) in
  let b_pick_req = 
    (if draft_pick = 1 then
      Some (Request (PickRequest (Blue, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_base_mons g))))
    else
      None) in
  GameState.set_phase g GameState.Draft;
  (None, (game_datafication g), r_pick_req, b_pick_req)


let  draft_phase s r b =    
(*Returns true if steammon is availible and player has enough money*)
  let valid_purchace smon color_p tbl : bool = 
      match (Table.mem tbl smon) with
      | true -> let poke = Table.find tbl smon in
        let cost = poke.cost in
        let monies = GameState.get_creds s (fst(color_p)) in
          if( monies >= cost) then true
          else false
      | false -> false
  in  

  let get_lowest_steammon color_p  tbl =
    let cost_checker (lowest_poke:steammon) (current_poke:steammon) : steammon =
      if (lowest_poke.cost <= current_poke.cost) then lowest_poke 
      else current_poke
    in
    GameState.set_creds s (fst(color_p)) 0;
    let lst = hash_to_list tbl in
      match lst with 
      hd::tl -> let poke = List.fold_left cost_checker hd lst in
              poke.species
      | _ -> failwith "Ran Out of Pokemon"         
    in 

  let purchace_steammon smon color_p tbl =
    let poke = Table.find tbl smon in
      let () = Table.remove tbl smon in
      GameState.set_draft_mons s tbl;
      GameState.add_reserve_steammon s (fst(color_p)) poke; 
      Netgraphics.add_update (UpdateSteammon (poke.species, poke.curr_hp, poke.max_hp, (fst(color_p))));
      let monies = GameState.get_creds s (fst(color_p)) in
        if (not(monies = 0)) then GameState.set_creds s (fst(color_p)) (monies - poke.cost);
        
      let datafif = game_datafication s in
      let finished = GameState.get_draft_finished s in
        match (finished, fst(color_p)) with
        |(false, Red) -> GameState.set_turn s Blue;
                        let b_req = Some (Request (PickRequest (Blue, (game_datafication s), 
                        (GameState.get_move_list s), (GameState.get_base_mons s)))) in
                        (None, datafif, None, b_req)
        |(false, Blue) -> GameState.set_turn s Red;
                        let r_req = Some (Request (PickRequest (Red, (game_datafication s), 
                        (GameState.get_move_list s), (GameState.get_base_mons s)))) in
                        (None, datafif, r_req, None)
        |(true, _) ->   GameState.set_phase s GameState.Inventory;
                        let inv_req = Some (Request (PickInventoryRequest(datafif))) in
                        (None, datafif, inv_req, inv_req)
     in

    let tbl = GameState.get_draft_mons s in
    let requester = GameState.get_turn s in
    let color_p =
      if (requester = Red) then 
        (Red,r) 
      else
        (Blue,b) 
    in
    match color_p with      
    | ( _ ,Action(PickSteammon nm)) -> let str =
      if (not(valid_purchace nm color_p tbl)) then (get_lowest_steammon color_p tbl) 
      else nm
    in
      (purchace_steammon str color_p tbl)
    | (_,_) -> (None, (game_datafication s), None, None)

let stock_inventories g rc bc =
  let cost_lst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_FULLHEAL; cCOST_REVIVE; 
                  cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in
  let default_inv = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE; cNUM_FULL_HEAL; 
                     cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in
  let stock_inventory_of (c: color) (inv: inventory) = 
    let cost = List.fold_left2 (fun worth item total -> worth*item + total) 0 cost_lst inv in
    if cost > cINITIAL_CASH then 
      GameState.set_inv g c default_inv
    else
      GameState.set_inv g c inv in
  let error_wrapper (c: color) = function
    | Action (PickInventory inv) -> stock_inventory_of c inv
    | DoNothing -> stock_inventory_of c default_inv
    | _ -> failwith "Neither PickInventory nor DoNothing" in
  error_wrapper Red rc;
  error_wrapper Blue bc;
  GameState.set_phase g GameState.Battle;
  (None, game_datafication g, Some (Request (StarterRequest (game_datafication g))), Some (Request (StarterRequest (game_datafication g))))


(****** STARTER PHASE BEGIN ******)

(*Tries to get an arbitrary_starter steammon, if all steammons have*)
(*fainted, check whether all the opposing steammons have fainted.*)
(*If so, game is a tie. If not, the opposing team wins. If there is*)
(*a steammon on the team that has not fainted, return that steammon.*)
let arbitrary_starter g c : steammon = 
  let steammon_list = GameState.get_steammon_list g c in
  let rec get_steammon lst = 
    match lst with
    | [] -> failwith "No steammons with player"
    | h::t -> 
        if h.curr_hp > 0 
        then h
        else get_steammon t in
  get_steammon steammon_list

(*Sets the active steammon from reserve_pool if the active steammon*)
(*faints or has not been chosen yet. If all reserve steammons are 
 *fainted, a game_result is set.*)
let battle_starter g rc bc : game_output = 
  let r_reserve_pool = GameState.get_reserve_pool g Red in
  let b_reserve_pool = GameState.get_reserve_pool g Blue in
  (match rc with
  | Action (SelectStarter rs) when (valid_steammon rs r_reserve_pool) ->
      let mon = Table.find r_reserve_pool rs in
      GameState.swap_active_steammon g Red mon;
      Netgraphics.add_update (SetChosenSteammon mon.species)
  | _ -> 
      let r_mon = arbitrary_starter g Red in
      GameState.swap_active_steammon g Red r_mon;
      Netgraphics.add_update (SetChosenSteammon r_mon.species));
  (match bc with
  | Action (SelectStarter bs) when (valid_steammon bs b_reserve_pool) ->
      let mon = Table.find b_reserve_pool bs in
      GameState.swap_active_steammon g Blue mon;
      Netgraphics.add_update (SetChosenSteammon mon.species)
  | _ -> 
      let b_mon = arbitrary_starter g Blue in
      GameState.swap_active_steammon g Blue b_mon;
      Netgraphics.add_update (SetChosenSteammon b_mon.species));
  let game_state = game_datafication g in
  (None, game_state, Some (Request (ActionRequest game_state)), 
    Some (Request (ActionRequest game_state)))

(****** STARTER PHASE END ******)


(****** BATTLE PHASE BEGIN *****)


(* status effects are only applied to the active Steammon *)
let handle_beginning_status (g: game) (mon: steammon) (team: color): unit = 
  let fate = Random.int 100 in
  let fate2 = Random.int 100 in
  match mon.status with
  | None -> ()
  | Some Paralyzed -> if fate < cPARALYSIS_CHANCE then 
			GameState.set_can_use_moves g team false
		      else let new_speed = (GameState.get_eff_speed g team) / cPARALYSIS_SLOW in
                           GameState.set_eff_speed g team mon new_speed;
			   add_update (AdditionalEffects 
					 [(StatModified (Spe, new_speed), team)])
  | Some Asleep -> if fate < cWAKE_UP_CHANCE then (GameState.set_status g team mon None;
 		     add_update (AdditionalEffects [(HealedStatus Asleep, team)])) 
		   else GameState.set_can_use_moves g team false
  | Some Frozen -> if fate < cDEFROST_CHANCE then (GameState.set_status g team mon None;
		     add_update (AdditionalEffects [(HealedStatus Frozen, team)])) 
		   else GameState.set_can_use_moves g team false
  | Some Confused -> if fate < cSNAP_OUT_OF_CONFUSION then (
		       GameState.set_status g team mon None;
		       add_update (AdditionalEffects [(HealedStatus Confused, team)]) )
		     else if fate2 < cSELF_ATTACK_CHANCE then 
		       GameState.set_will_attack_self g team true
		     else ()
  | Some Poisoned -> ()
  | Some Burned -> () (* burn weakness should be checked by seeing if 
                         status == burned when calculating damage *)

(*Need to check still alive after taking poison and burn damage *)
let handle_end_status g mon team : unit =
  match mon.status with 
  | None -> ()
  | Some Asleep -> GameState.set_can_use_moves g team true
  | Some Frozen -> GameState.set_can_use_moves g team true
  | Some Confused -> GameState.set_will_attack_self g team false
  | Some Paralyzed -> if GameState.get_can_use_moves g team = true then
			let new_speed = (GameState.get_eff_speed g team * cPARALYSIS_SLOW) in
			GameState.set_eff_speed g team mon new_speed;
			add_update (AdditionalEffects 
					 [(StatModified (Spe, new_speed), team)])
		      else GameState.set_can_use_moves g team true
  | Some Poisoned -> 
     let damage = int_of_float
       ((float_of_int(GameState.get_max_hp g team)) *. cPOISON_DAMAGE) in 
     GameState.set_hp g team mon (max((GameState.get_curr_hp g team) - damage) 0);
     add_update (AdditionalEffects [(DamagedByStatus (damage, Poisoned), team)])
  | Some Burned -> 
     let damage = int_of_float((float_of_int(GameState.get_max_hp g team)) *. cBURN_DAMAGE) in
     GameState.set_hp g team mon (max((GameState.get_curr_hp g team) - damage) 0);
     add_update (AdditionalEffects [(DamagedByStatus (damage, Burned), team)])

(*Use item. *)
let use_item (g: game) (c: color) (i: item) (mon_string: string) =
  let inv = GameState.get_inv g c in
  let compare_active_mon = function
    | Some smon -> (smon.species = mon_string)
    | None -> failwith "use item shouldn't recieve None for active teammon" in
  let is_active_mon = compare_active_mon (GameState.get_active_mon g c) in
  let steammon_list = GameState.get_steammon_list g c in
  let rec get_target_mon (mon_lst: steammon list) (name: string) : steammon option =
    match mon_lst with
    | hd::tl -> if (hd.species = name) then (Some hd) else get_target_mon tl name
    | _ -> None in
  match (get_target_mon steammon_list mon_string) with
  (*item used on a mon that isn't on the team.
    treat as missing message--no changes.*)
  | None -> () 
  (*item used on a mon that is on the team.*)
  | Some target_mon ->
  (*match item*)
    match i with 

    | Ether -> let new_inv = List.mapi (fun i a -> if i = 0 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
      if ((target_mon.curr_hp > 0) && ((List.nth inv 0) > 0)) then

        let use_ether move = 
          let new_pp = max (move.pp_remaining + 5) move.max_pp in
          {name = move.name;
          element = move.element;
          target = move.target;
          max_pp = move.max_pp;
          pp_remaining = new_pp;
          power = move.power;
          accuracy = move.accuracy;
          effects = move.effects} in 

        let new_ether_mon mon = 
          { species = mon.species; 
            curr_hp = mon.curr_hp; 
            max_hp = mon.max_hp;
            first_type = mon.first_type;
            second_type = mon.second_type;
            first_move = (use_ether mon.first_move);
            second_move = (use_ether mon.second_move);
            third_move = (use_ether mon.third_move);
            fourth_move = (use_ether mon.fourth_move);
            attack = mon.attack;
            spl_attack = mon.spl_attack;
            defense = mon.defense;
            spl_defense = mon.spl_defense;
            speed = mon.speed;
            status = mon.status;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_ether_mon target_mon in
        Netgraphics.add_update(Item("Ether", RestoredPP 5 , c, mon_string));
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      else ()

    | MaxPotion -> let new_inv = List.mapi (fun i a -> if i = 1 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
     (*not fainted. can use max potion*)
      if ((target_mon.curr_hp > 0) && ((List.nth inv 1) > 0)) then
        let new_max_potion_mon mon = 
          (*send CHANGE in health*)
          Netgraphics.add_update(Item("MaxPotion", Recovered (mon.max_hp - mon.curr_hp) , c, mon_string));
          Netgraphics.add_update(UpdateSteammon(mon.species, mon.max_hp, mon.max_hp, c));
          { species = mon.species; 
            curr_hp = mon.max_hp; 
            max_hp = mon.max_hp;
            first_type = mon.first_type;
            second_type = mon.second_type;
            first_move = mon.first_move;
            second_move = mon.second_move;
            third_move = mon.third_move;
            fourth_move = mon.fourth_move;
            attack = mon.attack;
            spl_attack = mon.spl_attack;
            defense = mon.defense;
            spl_defense = mon.spl_defense;
            speed = mon.speed;
            status = mon.status;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_max_potion_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      (*fainted mon. can't use max potion*)
      else ()

    | Revive -> let new_inv = List.mapi (fun i a -> if i = 2 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
     (*fainted. can use revive*)
      if ((target_mon.curr_hp <= 0) && ((List.nth inv 2) > 0)) then
        let new_revive_mon mon = 
          let new_hp = mon.max_hp/2 in
          (*send CHANGE in health*)
          Netgraphics.add_update(Item("Revive", Recovered new_hp, c, mon_string));
          Netgraphics.add_update(UpdateSteammon(mon.species, new_hp, mon.max_hp, c));
          { species = mon.species; 
            curr_hp = new_hp; 
            max_hp = mon.max_hp;
            first_type = mon.first_type;
            second_type = mon.second_type;
            first_move = mon.first_move;
            second_move = mon.second_move;
            third_move = mon.third_move;
            fourth_move = mon.fourth_move;
            attack = mon.attack;
            spl_attack = mon.spl_attack;
            defense = mon.defense;
            spl_defense = mon.spl_defense;
            speed = mon.speed;
            status = mon.status;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_revive_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      (*not fainted. can't use revive*)
      else ()


  | FullHeal -> let new_inv = List.mapi (fun i a -> if i = 3 then a else (a - 1))  inv in
    GameState.set_inv g c new_inv;
    if ((target_mon.curr_hp > 0) && ((List.nth inv 3) > 0)) then
      (match target_mon.status with
      (*no status to heal from. do nothing*)
      | None -> ()
      (*has an inflicted status*)
      | Some inflicted_status ->
          Netgraphics.add_update(Item("FullHeal", HealedStatus inflicted_status, c, mon_string));
          let new_fullheal_mon mon = 
          { species = mon.species; 
            curr_hp = mon.curr_hp; 
            max_hp = mon.max_hp;
            first_type = mon.first_type;
            second_type = mon.second_type;
            first_move = mon.first_move;
            second_move = mon.second_move;
            third_move = mon.third_move;
            fourth_move = mon.fourth_move;
            attack = mon.attack;
            spl_attack = mon.spl_attack;
            defense = mon.defense;
            spl_defense = mon.spl_defense;
            speed = mon.speed;
            status = None;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_fullheal_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon)
      else ()

  | XAttack -> let new_inv = List.mapi (fun i a -> if i = 4 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if (is_active_mon && (target_mon.curr_hp > 0) && ((List.nth inv 4) > 0)) then
      let new_xattack_mon mon = 
        let new_mods mods = 
          let new_attack_mod = max mods.attack_mod 6 in
          Netgraphics.add_update(Item("XAttack", StatModified (Atk, (new_attack_mod - mods.attack_mod)), c, mon_string));
          { attack_mod = new_attack_mod;
          defense_mod = mods.defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = mods.speed_mod} in
      { species = mon.species; 
        curr_hp = mon.curr_hp; 
        max_hp = mon.max_hp;
        first_type = mon.first_type;
        second_type = mon.second_type;
        first_move = mon.first_move;
        second_move = mon.second_move;
        third_move = mon.third_move;
        fourth_move = mon.fourth_move;
        attack = mon.attack;
        spl_attack = mon.spl_attack;
        defense = mon.defense;
        spl_defense = mon.spl_defense;
        speed = mon.speed;
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xattack_mon target_mon in
      GameState.set_active_mon g c (Some new_mon)
    else ()

  | XDefense -> let new_inv = List.mapi (fun i a -> if i = 5 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if (is_active_mon && (target_mon.curr_hp > 0) && ((List.nth inv 5) > 0)) then
      let new_xdefense_mon mon = 
        let new_mods mods = 
          let new_defense_mod = max mods.defense_mod 6 in
          Netgraphics.add_update(Item("XDefense", StatModified (Def, (new_defense_mod - mods.defense_mod)), c, mon_string));
          { attack_mod = mods.attack_mod;
          defense_mod = new_defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = mods.speed_mod} in
      { species = mon.species; 
        curr_hp = mon.curr_hp; 
        max_hp = mon.max_hp;
        first_type = mon.first_type;
        second_type = mon.second_type;
        first_move = mon.first_move;
        second_move = mon.second_move;
        third_move = mon.third_move;
        fourth_move = mon.fourth_move;
        attack = mon.attack;
        spl_attack = mon.spl_attack;
        defense = mon.defense;
        spl_defense = mon.spl_defense;
        speed = mon.speed;
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xdefense_mon target_mon in
      GameState.set_active_mon g c (Some new_mon)
    else ()

  | XSpeed -> let new_inv = List.mapi (fun i a -> if i = 6 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if (is_active_mon && (target_mon.curr_hp > 0) && ((List.nth inv 6) > 0)) then
      let new_xspeed_mon mon = 
        let new_mods mods = 
          let new_speed_mod = max mods.speed_mod 6 in
          Netgraphics.add_update(Item("XSpeed", StatModified (Spe, (new_speed_mod - mods.speed_mod)), c, mon_string));
          { attack_mod = mods.attack_mod;
          defense_mod = mods.defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = new_speed_mod} in
      { species = mon.species; 
        curr_hp = mon.curr_hp; 
        max_hp = mon.max_hp;
        first_type = mon.first_type;
        second_type = mon.second_type;
        first_move = mon.first_move;
        second_move = mon.second_move;
        third_move = mon.third_move;
        fourth_move = mon.fourth_move;
        attack = mon.attack;
        spl_attack = mon.spl_attack;
        defense = mon.defense;
        spl_defense = mon.spl_defense;
        speed = mon.speed;
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xspeed_mon target_mon in
      GameState.set_active_mon g c (Some new_mon)
    else ()

(* steammon if there exists at least one steammon that has not fainted,
 * None otherwise *)
let rec faint_check (lst:steammon list) : steammon option =
  match lst with
  | [] -> failwith "No steammons in list"
  | h::[] when h.curr_hp > 0 -> Some h
  | h::[] -> None
  | h::t when h.curr_hp > 0 -> Some h
  | h::t -> faint_check t

let switch_steammon g c mon : game_result option = 
  let player_reserves = GameState.get_reserve_pool g c in
  let s = match (GameState.get_active_mon g c) with
    | None -> failwith "SwitchSteammon when there is no active steammon"
    | Some smon -> smon in
  if valid_steammon mon player_reserves then
    (let switched_out_steammon = {
          species = s.species;
          curr_hp = s.curr_hp;
          max_hp = s.max_hp;
          first_type = s.first_type;
          second_type = s.second_type;
          first_move = s.first_move;
          second_move = s.second_move;
          third_move = s.third_move;
          fourth_move = s.fourth_move;
          attack = s.attack;
          spl_attack = s.spl_attack;
          defense = s.defense;
          spl_defense = s.spl_defense;
          speed = s.speed;
          status = s.status;
          mods = { attack_mod= 0;
                  defense_mod= 0;
                  spl_attack_mod= 0;
                  spl_defense_mod= 0;
                  speed_mod= 0;};
          cost = s.cost;
    } in 
    GameState.add_reserve_steammon g c switched_out_steammon;
    let new_steammon = Table.find player_reserves mon in
    GameState.remove_reserve_steammon g c new_steammon;
    GameState.set_active_mon g c (Some new_steammon);
    Netgraphics.add_update (SetChosenSteammon new_steammon.species);
    None)
  else 
    None

let get_move (mon:steammon) (s:string) : (move * int) option =
  let f_m = mon.first_move in
  let s_m = mon.second_move in
  let t_m = mon.third_move in
  let r_m = mon.fourth_move in
  if f_m.name = s then
    Some (f_m, 1)
  else if s_m.name = s then
    Some (s_m, 2)
  else if t_m.name = s then
    Some (t_m, 3)
  else if r_m.name = s then
    Some (r_m, 4)
  else
    None

let move_fail (m:move) : bool =
  if m.pp_remaining <= 0 then
    true
  else 
    false

let get_effectiveness sa s1 s2 : effectiveness =
  let (eff, _ ) = calculate_type_matchup sa (s1,s2) in
  eff


let reduce_move_pp (m:move) : move =
  print_string m.name;
  print_string " ";
  print_string "PP remaining: ";
  print_int m.pp_remaining;
  print_endline "";
  {
    name = m.name;
    element = m.element;
    target = m.target;
    max_pp = m.max_pp;
    pp_remaining = (m.pp_remaining - 1);
    power = m.power;
    accuracy = m.accuracy;
    effects = m.effects;
    }

let reduce_pp g c i =
  match (GameState.get_active_mon g c) with
  | None -> failwith "Reducing PP of non existent active steammon."
  | Some s -> 
      let (m1,m2,m3,m4) = 
        if i = 1 then
          ((reduce_move_pp s.first_move), s.second_move, s.third_move, s.fourth_move)
        else if i = 2 then
          (s.first_move, (reduce_move_pp s.second_move), s.third_move, s.fourth_move)
        else if i = 3 then
          (s.first_move, s.second_move, (reduce_move_pp s.third_move), s.fourth_move)
        else 
          (s.first_move, s.second_move, s.third_move, (reduce_move_pp s.fourth_move))
      in
      let updated_steammon = {
            species = s.species;
            curr_hp = s.curr_hp;
            max_hp = s.max_hp;
            first_type = s.first_type;
            second_type = s.second_type;
            first_move = m1;
            second_move = m2;
            third_move = m3;
            fourth_move = m4;
            attack = s.attack;
            spl_attack = s.spl_attack;
            defense = s.defense;
            spl_defense = s.spl_defense;
            speed = s.speed;
            status = s.status;
            mods = s.mods;
            cost = s.cost;
      } in 
    GameState.set_active_mon g c (Some updated_steammon)
    (*Netgraphics.add_update (SetChosenSteammon updated_steammon.species)*)

let miss_handler g from toward (m:move) =
  let failed_move_result = {
    name = m.name;
    element = m.element;
    from = from;
    toward = toward;
    damage = 0;
    hit = Miss;
    effectiveness = Regular;
    effects = [];
  } in
  Netgraphics.add_update (Move failed_move_result);
  None

let calc_multiplier (att_mon: steammon) (def_mon: steammon) (mv: move) =
  let stab = 
    match att_mon.first_type, att_mon.second_type with
    | None, Some typ when typ = mv.element -> cSTAB_BONUS
    | Some typ, None when typ = mv.element -> cSTAB_BONUS
    | Some typ1, Some typ2 when typ1 = mv.element || typ2 = mv.element -> cSTAB_BONUS 
    | _ -> 1. in
  let (eff, type_mult) = calculate_type_matchup mv.element (def_mon.first_type, 
							    def_mon.second_type) in
  let burn = if att_mon.status = Some Burned then cBURN_WEAKNESS 
	     else 1. in
  let rand = 
    float_of_int((Random.int (101 - cMIN_DAMAGE_RANGE)) + cMIN_DAMAGE_RANGE)  /. 100. in
  let _ = print_endline ("stab: " ^ string_of_float(stab) ^ " type_mult: " ^ (string_of_float type_mult) ^ " burn: " ^ string_of_float burn ^ " rand: " ^ string_of_float rand) in
  (stab *. type_mult *. burn *. rand, eff)

let move_hits mv = 
  match mv.target with
  | User when mv.power = 0 -> true
  | _ -> (Random.int 100) < mv.accuracy

let get_target att def mv c =
  match mv.target with
  | User -> (att, c)
  | Opponent -> (def, opp_color c)

let do_damage g target damage target_color = 
  if damage = 0 then ()
  else
    let new_hp = max (target.curr_hp - damage) 0 in 
    let _ = print_endline ("Old HP: " ^ string_of_int(target.curr_hp) ^ " New HP: " ^ (string_of_int new_hp)) in
    GameState.set_hp g target_color target new_hp 

let rec heal_status g color mon lst =
  match lst with
  | [] -> None
  | hd::tl -> match mon.status with
	      | Some x when x = hd -> GameState.set_status g color mon None;
				      Some hd
	      | _ -> heal_status g color mon tl

let handle_effects g (effect: effect) target target_color damage: effect_result option =
  match effect with
  | InflictStatus stat -> GameState.set_status g target_color target (Some stat);
			  Some (InflictedStatus stat)
  | StatModifier (stat, value) -> GameState.set_stat_modifier g target_color target stat value;
				  Some (StatModified (stat, value))
  | RecoverPercent percent -> 
     let new_hp = min(target.curr_hp + (int_of_float(
	 (float_of_int target.max_hp) *. (float_of_int percent) *. 0.01))) target.max_hp in
     GameState.set_hp g target_color target new_hp;
     Some (Recovered (new_hp - target.curr_hp))
  | Recoil percent -> 
     let new_hp = max (target.curr_hp - (int_of_float(
	 (float_of_int damage) *. (float_of_int percent) *. 0.01))) 0 in
     GameState.set_hp g target_color target new_hp;
     Some (Recoiled (target.curr_hp - new_hp))
  | DamagePercent percent -> 
    let new_hp = max (target.curr_hp - (int_of_float(
	 (float_of_int damage) *. (float_of_int percent) *. 0.01))) 0 in
     GameState.set_hp g target_color target new_hp;
     Some (Damaged (target.curr_hp - new_hp))
  | HealStatus lst -> (match heal_status g target_color target lst with
		      | None -> None
		      | Some stat -> Some (HealedStatus stat))
  | RestorePP value -> GameState.set_incr_pp g target_color target value;
			Some (RestoredPP value)

let traverse_effects g (mv:move) att_mon def_mon color damage = 
  let fate = Random.int 100 in
  let (lst, target, prob) = 
    match mv.effects with 
    | None -> ([], User, 0)
    | Some (x, y, z) -> (x,y,z) in
  let (targ_mon, targ_color) = 
    match target with
    | User -> (att_mon, color)
    | Opponent -> (def_mon, opp_color color) in
  let rec heal lst2 = 
    match lst2 with
    | [] -> []
    | hd::tl -> match handle_effects g hd targ_mon targ_color damage with
		| None -> heal tl
		| Some x -> (x, targ_color)::(heal tl) in
  if fate < prob then
    heal lst
  else []

let perform_struggle g c mon =
  let move_table = GameState.get_moves g in
  let struggle = Table.find move_table "Struggle" in
  let (mult, eff) = calc_multiplier mon mon struggle in
  let dmg = calculate_damage mon.attack mon.defense struggle.power mult in
  do_damage g mon dmg c;
  let effect_list = traverse_effects g struggle mon mon c dmg in
  let struggle_result = {
    name = struggle.name;
    element = struggle.element;
    from = c;
    toward = c;
    damage = dmg;
    hit = Hit;
    effectiveness = (get_effectiveness struggle.element mon.first_type mon.second_type);
    effects = effect_list;
  } in
  Netgraphics.add_update (UpdateSteammon (mon.species, mon.curr_hp, mon.max_hp, c));
  Netgraphics.add_update (Move struggle_result);
  None

let use_move g c move_str : game_result option =
  match (GameState.get_active_mon g c) with
  | None -> failwith "Called UseMove with no active steammon"
  | Some mon ->
      (match (get_move mon move_str)  with
      | None -> None
      | Some (m, i) -> 
          if (move_fail m) then
            perform_struggle g c mon
          else
            (match (GameState.get_active_mon g (opp_color c)) with
            | None -> None
            | Some opp_mon ->
              (reduce_pp g c i; 
              if move_hits m then
                let opp_mon = 
                  match GameState.get_active_mon g (opp_color c) with 
                  | None -> failwith "Opponent has no Steammon"
                  | Some mon -> mon in
                    let (mult, eff) = calc_multiplier mon opp_mon m in
                    assert (mult >= 0.);
		    let damage = 
                      if m.power = 0 then 
                        0 (*non damaging *)
                      else if is_special m.element then 
                        calculate_damage mon.spl_attack opp_mon.spl_defense m.power mult
                      else calculate_damage mon.attack opp_mon.defense m.power mult in
                    print_string "Move: ";
                    print_string m.name;
                    print_string " Damage: ";
                    print_int damage;
                    print_endline "";
		    assert (mon.spl_attack >= 0);
		    assert (opp_mon.spl_defense >= 0);
		    assert (mon.attack >= 0);
		    assert (opp_mon.defense >= 0);
		    assert (m.power >= 0);
		    assert (damage >= 0);
                    let (targ, targ_color) = get_target mon opp_mon m c in
                    do_damage g targ damage targ_color;
                    let effect_list = traverse_effects g m mon opp_mon c damage in
                    let move_update = {
                      name = m.name;
                      element = m.element;
                      from = opp_color targ_color;
                      toward = targ_color;
                      damage = damage;
                      hit = Hit;
                      effectiveness = eff;
                      effects = effect_list; } in
                    add_update (UpdateSteammon (mon.species, mon.curr_hp, mon.max_hp, c));
                    add_update (Move move_update);
                    None
              else 
                let targeted = if m.target = User then c else (opp_color c) in
                (*let targeted_mon = if targeted = c then mon else opp_mon in*)
                (*let eff = weakness mon.element opp_mon.element in *)
                miss_handler g c targeted m)))

(*Used to switch a steammon when a steammon has fainted and the given*)
(*mon is a valid_steammon*)
let switch_active g c s : game_result option = 
  let player_reserves = GameState.get_reserve_pool g c in
  let new_steammon = Table.find player_reserves s in
  GameState.remove_reserve_steammon g c new_steammon;
  GameState.set_active_mon g c (Some new_steammon);
  Netgraphics.add_update (SetChosenSteammon new_steammon.species);
  None

(*Used when current steammon has fainted and player sends an invalid*)
(*steammon to switch to. Game result maybe decided here.*)
let switch_active_arbitrary g c : game_result option = 
  match (faint_check (GameState.get_steammon_list g c)) with
  | None -> 
      (match (faint_check (GameState.get_steammon_list g (opp_color c))) with
      | None -> Some Tie 
      | Some _ -> Some (Winner (opp_color c)))
  | Some new_steammon ->
      GameState.remove_reserve_steammon g c new_steammon;
      GameState.set_active_mon g c (Some new_steammon);
      Netgraphics.add_update (SetChosenSteammon new_steammon.species);
      None

(* Checks whether the active steammon fainted by an action immediately
 * prior. (Status effect, Move, Inv use etc). Moves the fainted 
 * steammon to the reserve pool after resetting its modifiers.  *)
let active_faint_check g c : unit = 
  match (GameState.get_active_mon g c) with
  | None -> ()
  | Some s -> 
      (*print_string "Active steammon: ";*)
      (*print_string s.species;*)
      (*print_string " HP: ";*)
      (*print_int s.curr_hp;*)
      (*print_endline "";*)
      if s.curr_hp <= 0 then 
        (let fainted_steammon = {
              species = s.species;
              curr_hp = 0;
              max_hp = s.max_hp;
              first_type = s.first_type;
              second_type = s.second_type;
              first_move = s.first_move;
              second_move = s.second_move;
              third_move = s.third_move;
              fourth_move = s.fourth_move;
              attack = s.attack;
              spl_attack = s.spl_attack;
              defense = s.defense;
              spl_defense = s.spl_defense;
              speed = s.speed;
              status = s.status;
              mods = { attack_mod= 0;
                      defense_mod= 0;
                      spl_attack_mod= 0;
                      spl_defense_mod= 0;
                      speed_mod= 0;};
              cost = s.cost;
        } in 
        GameState.add_reserve_steammon g c fainted_steammon;
        Netgraphics.add_update 
          (UpdateSteammon (fainted_steammon.species, 0, s.max_hp, c));
        GameState.set_active_mon g c None
        )
      else
        ()

let failed_move g c mon move_str =
  match (get_move mon move_str) with
  | None -> ()
  | Some (mv, _) ->
      let m = {
        name = mv.name;
        element = mv.element;
        from = c;
        toward =  (if mv.target = User then c else (opp_color c));
        damage = 0;
        hit = (match mon.status with 
              | None -> failwith "Failing non failing move"
              | Some stat -> (Failed stat));
        effectiveness = Regular;
        effects = [];
        } in
      Netgraphics.add_update (Move m)

let battle_action g c comm : game_result option = 
  let player_reserves = GameState.get_reserve_pool g c in
  match (GameState.get_active_mon g c) with
  (* Active steammon has fainted, require SelectStarter *)
  | None -> 
      (match comm with 
      | Action (SelectStarter s) when valid_steammon s player_reserves -> 
          switch_active g c s 
      | _ -> switch_active_arbitrary g c)
  | Some mon ->
      (match comm with
      | Action (UseItem (i, s)) -> (use_item g c i s); None
      | Action (UseMove s) when (GameState.get_can_use_moves g c) -> 
          use_move g c s
      | Action (UseMove s) -> (failed_move g c mon s); None
      | Action (SwitchSteammon s) -> switch_steammon g c s
      | _ -> None)

let battle_phase g rc bc : game_output = 
  (* Apply the status effects, handle the outstanding actions and
   * then send out requests depending on whose turn it is *)
  (match (GameState.get_active_mon g Red) with
  | Some mon -> handle_beginning_status g mon Red
  | None -> () );
  (match (GameState.get_active_mon g Blue) with
  | Some mon -> handle_beginning_status g mon Blue
  | None -> () );
  
  let (faster_action, slower_action, faster_color) = 
    if (GameState.get_eff_speed g Red) > (GameState.get_eff_speed g Blue) 
    then (rc, bc, Red) 
    else (bc, rc, Blue) in
  Netgraphics.add_update (SetFirstAttacker faster_color);
  let result = battle_action g faster_color faster_action in
  match result with
  | Some res -> (Some res, (game_datafication g), None, None)
  | None -> 
      active_faint_check g faster_color;
      let result2 = battle_action g (opp_color faster_color) slower_action in
      match result2 with
      | Some res -> (Some res, (game_datafication g), None, None)
      | None ->
          active_faint_check g (opp_color faster_color);
          (match (GameState.get_active_mon g Red) with
          | Some mon -> handle_end_status g mon Red
          | None -> () );
          (match (GameState.get_active_mon g Blue) with
          | Some mon -> handle_end_status g mon Blue
          | None -> () );
          let r_req = 
            if (GameState.get_active_mon g Red) = None then
              Request (StarterRequest (game_datafication g))
            else 
              Request (ActionRequest (game_datafication g)) in
          let b_req = 
            if (GameState.get_active_mon g Blue) = None then
              Request (StarterRequest (game_datafication g))
            else 
              Request (ActionRequest (game_datafication g)) in
          (None, (game_datafication g), Some r_req, Some b_req)

(****** BATTLE PHASE END   *****)


let handle_step (g:game) (rc:command) (bc:command) : game_output =
  (* Handle status effects that occur at end of turn *)
  let current_phase = GameState.get_phase g in
  match current_phase with
  | GameState.TeamName -> team_phase g rc bc
  | GameState.Draft -> draft_phase g rc bc
  | GameState.Inventory -> stock_inventories g rc bc
  | GameState.Starter -> battle_starter g rc bc
  | GameState.Battle -> battle_phase g rc bc        

let init_game () : game * request * request * move list * steammon list =
  (* Creating a blank state for the beginning of the game *)
  let init_state = !game_instance in

  (* Loading moves list and Steammon list *)
  let mvs = GameState.get_move_list init_state in
  let mons = GameState.get_base_mons init_state in

  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)

 


  



