open Team
open Definitions
open Constants

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "random_request_bot" 

let _ = Random.self_init ()

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  let nbr = Random.int 5 in

  match nbr with
    | 0 -> SendTeamName(name)
    | 1 -> SelectStarter("Imaginary species")
    | 2 -> PickSteammon("Imaginary species")
    | 3 -> UseMove("Imaginary Move")
	  | 4 -> PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
    |_-> failwith "what is with this generator?"
let () = run_bot handle_request
