(* emblemmasta.ml
   A (relatively) simple program to calculate stats and levels for
   Eddie's Fire Emblem tabletop game.
   Uses lablGTK2, and the OCaml Num library for ratios.

   ...you know, instead of figuring out all the growths dynamically, maybe 
   it would have just been simpler to generate a bunch of tables of stats, 
   and just leave it at that?
   Oh well.

   Simon Heath
   26/5/2005
*)

open Num;;

let growth = Array.create 7 (num_of_string "1/6");;
let hpgrowth = Array.create 5 (num_of_string "1/2");;

let initGrowth () =
  growth.(1) <- (num_of_string "1/5");
  growth.(2) <- (num_of_string "1/4");
  growth.(3) <- (num_of_string "1/3");
  growth.(4) <- (num_of_string "1/2");
  growth.(5) <- (num_of_string "2/3");
  growth.(6) <- (num_of_string "1/1");

  (* XXX: Values aren't quite right here *)
  hpgrowth.(1) <- (num_of_string "2/3");
  hpgrowth.(2) <- (num_of_string "1/1");
  hpgrowth.(3) <- (num_of_string "3/2");
  hpgrowth.(4) <- (num_of_string "2/1");
;;

type character = {
  mutable level : int;
 (* Not the growths themselves, but the number of points put into them *)
  mutable hpg : int;
  mutable strg : int;
  mutable magg : int;
  mutable spdg : int;
  mutable sklg : int;
  mutable intg : int;
  mutable cong : int;

  mutable hp : int;
  mutable str : int;
  mutable mag : int;
  mutable spd : int;
  mutable skl : int;
  mutable int : int;
  mutable con : int;

  mutable points : int;
}

let makeChar () = {
  level = 1;
  hpg = 0;
  strg = 0;
  magg = 0;
  spdg = 0;
  sklg = 0;
  intg = 0;
  cong = 0;

  hp = 15;
  str = 0;
  mag = 0;
  spd = 3;
  skl = 3;
  int = 4;
  con = 4;

  points = 0;
};;

type stats =
    Hp
  | Str
  | Mag
  | Spd
  | Skl
  | Int
  | Con
;;


let setStat char stat num =
  match stat with
    Hp ->
      char.hpg <- num;
      if char.hpg > 4 then char.hpg <- 4
      else if char.hpg < 0 then char.hpg <- 0
  | Str ->
      char.strg <- num;
      if char.strg > 6 then char.strg <- 6
      else if char.strg < 0 then char.strg <- 0
  | Mag ->
      char.magg <- num;
      if char.magg > 6 then char.magg <- 6
      else if char.magg < 0 then char.magg <- 0
  | Spd ->
      char.spdg <- num;
      if char.spdg > 6 then char.spdg <- 6
      else if char.spdg < 0 then char.spdg <- 0
  | Skl ->
      char.sklg <- num;
      if char.sklg > 6 then char.sklg <- 6
      else if char.sklg < 0 then char.sklg <- 0
  | Int ->
      char.intg <- num;
      if char.intg > 6 then char.intg <- 6
      else if char.intg < 0 then char.intg <- 0
  | Con ->
      char.cong <- num;
      if char.cong > 6 then char.cong <- 6
      else if char.cong < 0 then char.cong <- 0
;;

let statLevel g lev =
  let inc =
    match g with
	0 -> lev / 6
      | 1 -> lev / 5
      | 2 -> lev / 4
      | 3 -> lev / 3
      | 4 -> lev / 2
      | 5 -> lev * 2 / 3
      | 6 -> lev
      | _ -> raise (Failure "statLevel")
  in
    inc

;;

let hpLevel g lev =
  let inc =
    match g with
	0 -> lev / 2
      | 1 -> lev * 2 / 3
      | 2 -> lev
      | 3 -> lev * 3 / 2
      | 4 -> lev * 2
      | _ -> raise (Failure "hpLevel")
  in
    inc
;;

let levelChar char lev =
  char.level <- lev;
  char.hp <- 15 + char.hpg + (hpLevel char.hpg char.level);
  let str = char.strg + (statLevel char.strg char.level)
  and mag = char.magg + (statLevel char.magg char.level)
  and spd = 3 + char.spdg + (statLevel char.spdg char.level)
  and skl = 3 + char.sklg + (statLevel char.sklg char.level)
  and int = 4 + char.intg + (statLevel char.intg char.level)
  and con = 4 + char.cong + (statLevel char.cong char.level) in
    char.str <- if str > 25 then 25 else str;
    char.mag <- if mag > 25 then 25 else mag;
    char.spd <- if spd > 25 then 25 else spd;
    char.skl <- if skl > 25 then 25 else skl;
    char.int <- if int > 25 then 25 else int;
    char.con <- if con > 25 then 25 else con;

    char.points <- char.hpg + char.strg + char.magg + char.spdg +
    char.sklg + char.intg + char.cong;
;;

let printChar char =
  Printf.printf "HP: %d, %d\n" char.hp char.hpg;
  Printf.printf "STR: %d, %d\n" char.str char.strg;
  Printf.printf "MAG: %d, %d\n" char.mag char.magg;
  Printf.printf "SPD: %d, %d\n" char.spd char.spdg;
  Printf.printf "SKL: %d, %d\n" char.skl char.sklg;
  Printf.printf "INT: %d, %d\n" char.int char.intg;
  Printf.printf "CON: %d, %d\n" char.con char.cong;
  Printf.printf "Points spent: %d\n" char.points;
  flush stdout;
;;

let refreshChar char =
  levelChar char char.level;
;;






let main () =
  initGrowth ();
  let c = makeChar () in


  let window = GWindow.window ~title:"Emblem Masta" 
		 (*~width:300 ~height:300 *) ~border_width:5 () in
    ignore (window#connect#destroy ~callback:GMain.Main.quit);
    (*
      let box = GPack.hbox ~packing: window#add () in
      let button = GButton.button ~label: "Button 1" ~packing: box#pack () in
      let statframe = GBin.frame ~label: "Stats" ~shadow_type: `ETCHED_OUT 
      ~packing: box#pack () in
    *)
    (* Create all the containers and set them up right.
       We have a hbox for the whole thing, a frame and a table inside that,
       and another table in the frame. *)       
    let box = GPack.hbox ~packing: window#add () in 
    let statframe = GBin.frame ~label: "Stats" ~shadow_type: `ETCHED_OUT 
		      ~packing: box#pack () in
    let stattable = GPack.table ~rows: 8 ~columns: 4 ~homogeneous: false
		      ~packing: statframe#add 
		      ~row_spacings: 5 ~col_spacings: 5 () in 
    let displaytable = GPack.table ~rows: 2 ~columns: 8 ~homogeneous: false
			 ~packing: box#pack 
			 ~row_spacings: 5 ~col_spacings: 5 () in 


    (* Set up statframe adjustments *)
    (* Weirdly enough, these don't actually seem to adjust... *)
    let hpadj = GData.adjustment ~value: 0.0 ~upper: 4.0 ~lower: 0.0 ()
    and stradj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and magadj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and skladj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and spdadj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and intadj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and conadj = GData.adjustment ~value: 0.0 ~upper: 6.0 ~lower: 0.0 ()
    and levadj = GData.adjustment ~value: 0.0 ~upper: 30.0 ~lower: 0.0 () in

    (* Set up statframe spinners *)      
    let hpsp = GEdit.spin_button ~adjustment: hpadj ~wrap: false ()
    and strsp = GEdit.spin_button ~adjustment: stradj ~wrap: false ()
    and magsp = GEdit.spin_button ~adjustment: magadj ~wrap: false ()
    and spdsp = GEdit.spin_button ~adjustment: spdadj ~wrap: false ()
    and sklsp = GEdit.spin_button ~adjustment: skladj ~wrap: false ()
    and consp = GEdit.spin_button ~adjustment: conadj ~wrap: false ()
    and intsp = GEdit.spin_button ~adjustment: intadj ~wrap: false ()
    and levsp = GEdit.spin_button ~adjustment: levadj ~wrap: false () in

    (* Set up statframe textareas for stat increments*)
    let hpinc = GEdit.entry ~text: "1/2" ~editable: false ()
    and strinc = GEdit.entry ~text: "1/6" ~editable: false ()
    and maginc = GEdit.entry ~text: "1/6" ~editable: false ()
    and spdinc = GEdit.entry ~text: "1/6" ~editable: false ()
    and sklinc = GEdit.entry ~text: "1/6" ~editable: false ()
    and coninc = GEdit.entry ~text: "1/6" ~editable: false ()
    and intinc = GEdit.entry ~text: "1/6" ~editable: false ()
    in
      
    (* Set up statframe textareas for stats *)
    let hpstat = GEdit.entry ~text: "15" ~editable: false ()
    and strstat = GEdit.entry ~text: "0" ~editable: false ()
    and magstat = GEdit.entry ~text: "0" ~editable: false ()
    and spdstat = GEdit.entry ~text: "3" ~editable: false ()
    and sklstat = GEdit.entry ~text: "3" ~editable: false ()
    and constat = GEdit.entry ~text: "4" ~editable: false ()
    and intstat = GEdit.entry ~text: "4" ~editable: false ()
    in

    (* Set up displayframe textareas for atk, evd, mdef, and dblstrike *)
    let atkdisp = GEdit.entry ~text: "70" ~editable: false ()
    and evddisp = GEdit.entry ~text: "0" ~editable: false ()
    and mdefdisp = GEdit.entry ~text: "0" ~editable: false ()
    and dblsdisp = GEdit.entry ~text: "1" ~editable: false ()
    and ptdisp = GEdit.entry ~text: "0" ~editable: false ()
    in

    (* Link the adjustments to the char object, so one changes if the 
       other does 
       Okay.  So how this has to work: Each function takes the stat
       and value from the spinner.  It sets the stat value, then calls the
       refresh func.  That calculates the growth ratio, multiplies it by
       the level, floors it, adds the result to the base and the number of
       the spinner.  Then it sets the stat and growth labels appropriately.
    *)

    (* These are the functions that really manipulate the GUI. *)
    let updateDisplay () =
      refreshChar c;
      hpstat#set_text (string_of_int c.hp);
      strstat#set_text (string_of_int c.str);
      magstat#set_text (string_of_int c.mag);
      spdstat#set_text (string_of_int c.spd);
      sklstat#set_text (string_of_int c.skl);
      intstat#set_text (string_of_int c.int);
      constat#set_text (string_of_int c.con);

      atkdisp#set_text (string_of_int (70 + (c.skl * 5)));
      evddisp#set_text (string_of_int (c.spd * 5));
      mdefdisp#set_text (string_of_int (c.int / 2));
      dblsdisp#set_text (string_of_int (int_of_float 
					  ((float_of_int c.spd) /. 1.5)));
      ptdisp#set_text (string_of_int c.points);      
    in
    let resetDisplay () =
      hpsp#set_value 0.0;
      strsp#set_value 0.0;
      magsp#set_value 0.0;
      spdsp#set_value 0.0;
      sklsp#set_value 0.0;
      intsp#set_value 0.0;
      consp#set_value 0.0;
      levsp#set_value 0.0;
      updateDisplay ();
    in

    let cb stat vl incentry statentry () = 
      setStat c stat (int_of_float vl#value);
      updateDisplay ();
    in

      (* Set up callbacks to change the GUI when the stat and level spinners
	 change. *)
      ignore (hpsp#connect#changed ~callback: (cb Hp hpsp hpinc hpstat));
      ignore (strsp#connect#changed ~callback: (cb Str strsp strinc strstat));
      ignore (magsp#connect#changed ~callback: (cb Mag magsp maginc magstat));
      ignore (spdsp#connect#changed ~callback: (cb Spd spdsp spdinc spdstat));
      ignore (sklsp#connect#changed ~callback: (cb Skl sklsp sklinc sklstat));
      ignore (intsp#connect#changed ~callback: (cb Int intsp intinc intstat));
      ignore (consp#connect#changed ~callback: (cb Con consp coninc constat));
      ignore (levsp#connect#changed ~callback:
		(fun () -> 
		   levelChar c (int_of_float levsp#value); 
		   updateDisplay (); ));
      




      

      (* fill statframe *)
      (* Labels *)
      (let l = GMisc.label ~text: "HP:" () in
	 stattable#attach ~left: 0 ~top: 0 l#coerce;
	 let l = (GMisc.label ~text: "STR:" ()) in
	   stattable#attach ~left: 0 ~top: 1 l#coerce;
	   let l = (GMisc.label ~text: "MAG" ()) in
	     stattable#attach ~left: 0 ~top: 2 l#coerce;
	     let l = (GMisc.label ~text: "SPD:" ()) in
	       stattable#attach ~left: 0 ~top: 3 l#coerce;
	       let l = (GMisc.label ~text: "SKL:" ()) in
		 stattable#attach ~left: 0 ~top: 4 l#coerce;
		 let l = (GMisc.label ~text: "INT:" ()) in
		   stattable#attach ~left: 0 ~top: 5 l#coerce;
		   let l = (GMisc.label ~text: "CON:" ()) in
		     stattable#attach ~left: 0 ~top: 6 l#coerce;
		     
		     let l = (GMisc.label ~text: "Attack:" ()) in
		       displaytable#attach ~left: 0 ~top: 1 l#coerce;
		       let l = (GMisc.label ~text: "Evade:" ()) in
			 displaytable#attach ~left: 0 ~top: 2 l#coerce;
			 let l = (GMisc.label ~text: "MDef:" ()) in
			   displaytable#attach ~left: 0 ~top: 3 l#coerce;
			   let l = (GMisc.label ~text: "Doublestrike:" ()) in
			     displaytable#attach ~left: 0 ~top: 4 l#coerce;
			   let l = (GMisc.label ~text: "Points spent:" ()) in
			     displaytable#attach ~left: 0 ~top: 5 l#coerce;
      );

      
      (* Spinners *)
      stattable#attach ~left: 1 ~top: 0 hpsp#coerce;
      stattable#attach ~left: 1 ~top: 1 strsp#coerce;
      stattable#attach ~left: 1 ~top: 2 magsp#coerce;
      stattable#attach ~left: 1 ~top: 3 spdsp#coerce;
      stattable#attach ~left: 1 ~top: 4 sklsp#coerce;
      stattable#attach ~left: 1 ~top: 5 intsp#coerce;
      stattable#attach ~left: 1 ~top: 6 consp#coerce;

      (* Stat entries *)
      stattable#attach ~left: 2 ~top: 0 hpstat#coerce;
      stattable#attach ~left: 2 ~top: 1 strstat#coerce;
      stattable#attach ~left: 2 ~top: 2 magstat#coerce;
      stattable#attach ~left: 2 ~top: 3 spdstat#coerce;
      stattable#attach ~left: 2 ~top: 4 sklstat#coerce;
      stattable#attach ~left: 2 ~top: 5 intstat#coerce;
      stattable#attach ~left: 2 ~top: 6 constat#coerce;

      (* Growth entries *)


      (* Misc display table stuff *)
      let l = (GMisc.label ~text: "Level:" ()) in
	displaytable#attach ~left: 0 ~top: 0 l#coerce;
	displaytable#attach ~left: 1 ~top: 0 levsp#coerce;
	displaytable#attach ~left: 1 ~top: 1 atkdisp#coerce;
	displaytable#attach ~left: 1 ~top: 2 evddisp#coerce;
	displaytable#attach ~left: 1 ~top: 3 mdefdisp#coerce;
	displaytable#attach ~left: 1 ~top: 4 dblsdisp#coerce;
	displaytable#attach ~left: 1 ~top: 5 ptdisp#coerce;
	
	(* Quit and clear controls *)
	let rb = GButton.button ~label: "Reset" ()
	and qb = GButton.button ~label: "Quit" () in
	  rb#connect#clicked ~callback: resetDisplay;
	  qb#connect#clicked ~callback: GMain.Main.quit;

	  displaytable#attach ~left: 1 ~top: 6 rb#coerce;
	  displaytable#attach ~left: 1 ~top: 7 qb#coerce;

	window#show ();
	GMain.Main.main ();
;;


let _ = 
  Printexc.print main ()
;;
   
