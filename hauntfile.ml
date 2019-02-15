open Data
open Players
open Command

(*[team st i] is all players in [st] on team [i]*)
let team st (i: int) =
  List.filter (fun p -> p.team_id = i) st.players

(*[alive_team team] returns true if at least one member of [team] is alive *)
let alive_team (team : player list) =
  List.exists (fun p -> p.alive) team

let single_living_team plyrs =
  let alive = List.filter (fun p -> p.alive) plyrs in
  let team_ids = List.map (fun p -> p.team_id) alive in
  match team_ids with
  | [h] -> Some h
  | _ -> None

(*[count_items team] returns total number of cards held
  by all members of [team]*)
let count_items (team : player list) =
  List.fold_left (fun (x: int) (y : player) -> x + (y.cards |> List.length))
    0 team

(*[count_stats team stat] is total sum of [stat] values of all members of
  [team]*)
let count_stats (team : player list) stat =
  List.fold_left (fun (x: int) (y : player) -> x + (get_stat_value y stat))
    0 team

let default_haunt = {
  haunt_name = "default";
  haunt_rules = "No rules";
  win_condition = (fun x -> None);
  win_text = (fun team_id ->
      "Team "^string_of_int team_id^" has won the game!"
    );
  rules = (fun x -> ());
  teams = (fun st -> let pl = curr_plyr st in pl.team_id <-1);
}

let list_rep value rep =
  let rec helper lst n =
    if n = 0 then lst else helper (value :: lst) (n-1) in
  helper [] rep

(*[set_all_stats plyr v] changes all of [plyr]'s stats to [v]*)
let set_all_stats plyr v =
  set_stat plyr v Sanity;
  set_stat plyr v Knowledge;
  set_stat plyr v Might;
  set_stat plyr v Speed

  let turn_into_jack plyr strength =
    plyr.character <- {
      name = "Jack The Ripper";
      desc = "run";
      sanity = list_rep strength 8;
      knowledge = list_rep strength 8;
      might = list_rep strength 8;
      speed = list_rep strength 8;
      sanity_initial = 1;
      knowledge_initial = 1;
      might_initial = 1;
      speed_initial = 1;
    };
    set_all_stats plyr 1

let (haunt_outlines : haunt list) = [
  {
    default_haunt with
    haunt_name = "null haunt";
    haunt_rules = "no rules. Immediate win for team 0";
    win_condition = (fun x -> Some 0);
  };
  {
    default_haunt with
    haunt_name = "battle one super";
    haunt_rules = "player who started this gets lots of power. Everyone else
is on the other team. FIght to the deat";
    win_condition = (fun st -> single_living_team st.players);
    win_text = (fun team_id ->
        "Team "^string_of_int team_id^" has won the game as the last team standing!"
      );
    rules = fun (st: game_state) ->
      let super = curr_plyr st in
      List.iter (fun x -> change_stat super st 8 x)
        [Knowledge; Might; Speed; Sanity]
  };
  {
    haunt_name = "Slow motion";
    haunt_rules = "All players for themselves.
        Everyone's speed is 3 from death. Winner is whoever gets to
        Zoo.";
    teams = (fun st -> List.iteri (fun i p -> p.team_id <- i) st.players);
    win_condition = (fun st ->
        let w = List.find_opt
            (fun p -> p.current_room.tile.tile_name = "Zoo") st.players in
        match w with
        | Some p -> Some p.team_id
        | None -> None);
    win_text = (fun team_id ->
        "Team "^string_of_int team_id^" has made it to the zoo and won the game!"
      );
    rules = (fun st ->
        List.iter (fun x -> set_stat x 2 Speed) st.players;
        st.display_teams <- true
      )
  };
  {
    default_haunt with
    haunt_name = "Jack The Ripper";
    haunt_rules = (
      "The player who activated the haunt has channeled\n"^
      "evil energy to become Jack The Ripper.\n\n"^
      "He can only be defeated by the Cursed Dagger"
    );
    win_condition = (fun st ->
        match single_living_team st.players with
        | Some 1 -> Some 1
        | _ -> None
      );
    win_text = (fun team_id ->
        match team_id with
        | 1 -> "Jack The Ripper has killed everyone and won the game!"
        | 0 -> "The survivors have permanantly put Jack The Ripper\n"^
               "six feet under and won the game!"
        | _ -> ""
      );
    rules = (fun st ->
        let plyr = curr_plyr st in
        turn_into_jack plyr 2;
        set_all_stats plyr 1;
        st.start_round_fun <- (fun s ->
            if not plyr.alive then (
              plyr.alive <- true;
              plyr.current_room <- s.start_room;
              turn_into_jack plyr ((List.hd plyr.character.might) + 1);
              set_all_stats plyr 1
            )
          );
        st.on_died_fun <- (fun s plyr item_used ->
            if plyr.character.name <> "Jack The Ripper" then () else
            match item_used with
            | Some card ->
              if card.card_name = "Cursed Dagger" then (
                st.winner <- Some 0;
                st.force_end_turn <- true;
                st.start_round_fun <- (fun s -> ())
              )
            | None -> ()
          )
      )
  };
  {
    default_haunt with
    haunt_name = "Collect the Items but stay sane";
    haunt_rules = "This player and one other are evil. The others are on the
      other team. Whichever team cumulatively has 5 items and 8 sanity wins";
    teams = (fun st -> List.iteri (fun i x -> if i < 2
                                    then x.team_id <- 1) st.players);
    win_condition = (fun st ->
        let t0 = team st 0 in let t1 = team st 1 in
        match (count_items t0 > 4, count_items t1 > 4) with
        | (true, _) when count_stats t0 Sanity > 7 -> Some 0
        | (_, true) when count_stats t1 Sanity > 7 -> Some 1
        | _ -> None)
  };
  {
    default_haunt with
    haunt_name = "Race to death";
    haunt_rules = "Teams are split into two, the first team to have both
players die wins";
    teams = (fun st -> List.iteri (fun i x -> if i > 1
                                    then x.team_id <- 1) st.players);
    win_condition = (fun st ->
        let alive_list = List.map (fun p -> p.alive) st.players  in
        match alive_list with
        | [false; false; _; _] -> Some 0
        | [_ ; _; false; false] -> Some 1
        | _ -> None);
    rules = (fun st ->
        st.display_teams <- true
    )
  }
]

(*[act_haunt_from_h st h] displays name and rules of [h], then changes
  [st] so haunt [h]'s teams, rules, and win condition are in play  *)
let act_haunt_from_h st h =
  Uifile.render_popup_text h.haunt_name h.haunt_rules;
  let _ = wait_for_any in
  h.teams st; h.rules st;
  st.haunt <- Some h

(*[activate_haunt st card] selects haunt based on [card], then activates it in [st] *)
let activate_haunt st card =
  st.haunt_activated <- true;
  let index =
    match card.card_modifier with
    | Omen id -> id
    | _ -> 0 in
  act_haunt_from_h st (List.nth haunt_outlines index)
