open Data
open Players
open Command
open Uifile
open Dicefile

(* the default time *)
let default_tile : tile = {
  tile_name = "";
  tile_name_abbrev = "";
  tile_desc = "";
  tile_modifier = None;
  areas = [Ground];
  tile_left = false;
  tile_right = false;
  tile_up = false;
  tile_down = false;
  enter_function = (fun _ -> ());
  exit_function = (fun _ -> ());
  end_function = (fun _ -> ());
}

(* all tiles that can be placed in the game *)
let all_tiles : tile list = [
  {
    default_tile with
    tile_name = "Main";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc =
      "Welcome to the mansion"
  };
  {
    default_tile with
    tile_name = "Flat";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc =
      "An open area"
  };
  {
    default_tile with
    tile_name = "Spider";
    tile_name_abbrev = "Spidr";
    tile_left = true;
    tile_right = true;
    tile_desc = (
      "You are caught in a bunch of giant cob webs!
      Do a Speed roll to see if you may pass through.\n"^
      "[0]     Lose 1 speed and end your turn\n"^
      "[1 - 2] End your turn                 \n"^
      "[3+]    Pass through safely           ");
    enter_function = (fun state ->
        let plyr = curr_plyr state in
        Uifile.render_game state;
        Uifile.render_popup_text plyr.current_room.tile.tile_name plyr.current_room.tile.tile_desc;
        let speed_roll = Command.roll_by_stat plyr Speed in
        Uifile.render_popup_text ("Rolled "^(string_of_int speed_roll)) "";
        if speed_roll == 0 then (
          change_stat plyr state (-1) Speed;
          state.force_end_turn <- true
        ) else if speed_roll >= 1 && speed_roll <= 2 then (
          state.force_end_turn <- true
        ) else (
        );
        wait_for_click ()
      )
  };
  {
    default_tile with
    tile_name = "Maze";
    tile_left = true;
    tile_right = true;
    tile_desc = (
      "The walls seem to move around you. Getting out\n"^
      "of here isn't going to be easy.\n"^
      "You must pass a Knowledge roll in order to leave.\n"^
      "[0 - 1] Remain in the maze\n"^
      "[2+]    Escape safely     ");
    exit_function = (fun state ->
        let plyr = curr_plyr state in
        Uifile.render_game state;
        Uifile.render_popup_text plyr.current_room.tile.tile_name plyr.current_room.tile.tile_desc;
        let knowledge_roll = Command.roll_by_stat plyr Knowledge in
        Uifile.render_popup_text ("Rolled "^(string_of_int knowledge_roll)) "";
        if knowledge_roll <= 1 then (
          state.force_end_turn <- true
        );
        wait_for_click ()
      )
  };
  {
    default_tile with
    tile_name = "Pool";
    tile_desc = (
      "Where'd the lifeguard go? There are murky shapes swimming \n" ^
      "below you. You must pass a speed roll to escape.\n" ^
      "[0 - 1] Keep treading water, lost 1 sanity\n" ^
      "[2+]    Swim out safely                   ");
    enter_function = (fun state ->
        let plyr = curr_plyr state in
        Uifile.render_game state;
        Uifile.render_popup_text plyr.current_room.tile.tile_name plyr.current_room.tile.tile_desc;
        let speed_roll = Command.roll_by_stat plyr Speed in
        Uifile.render_popup_text ("Rolled " ^(string_of_int speed_roll)) "";
        if speed_roll <= 1 then (
          change_stat plyr state (-1) Sanity;
          state.force_end_turn <- true
        );
        wait_for_click ()
      );
    tile_right = true;
    tile_up = true;
    tile_down = true;
  };
  {
    default_tile with
    tile_name = "Library";
    tile_name_abbrev = "Libry";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc = (
      "A grand library filled with endless rows of books\n\n"^
      "Once per game, ending your turn here grants +1 Knowledge");
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 1 Knowledge
      )
  };
  {
    default_tile with
    tile_name = "Gym";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc = (
      "An impressive gym filled with workout equipment.\n\n"^
      "Once per game, ending your turn here grants +1 Might");
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 1 Might
      )
  };
  {
    default_tile with
    tile_name = "Track";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc = (
      "An entire olympic size track in this room.\n\n"^
      "Once per game, ending your turn here grants +1 Speed");
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 1 Speed
      )
  };
  {
    default_tile with
    tile_name = "Food";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
    tile_desc = (
      "This room is filled with comfort food.\n\n"^
      "Once per game, ending your turn here grants +1 Sanity");
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 1 Sanity
      )
  };
  {
    default_tile with
    tile_name = "Comp";
    tile_modifier = Some (Omen 0);
    tile_left = true;
    tile_down = true;
  };
  {
    default_tile with
    tile_name = "Halls";
    tile_left = true;
    tile_right = true;
    tile_up = true;
    tile_down = true;
  };
  {
    default_tile with
    tile_name = "Foyer";
    tile_left = true;
    tile_right = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Hall";
    tile_up = true;
    tile_down = true;
  };
  {
    default_tile with
    tile_name = "Land";
    tile_modifier = Some (Omen 0);
    tile_left = true;
    tile_right = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Crypt";
    tile_left = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Vault";
    tile_down = true;
    tile_desc = (
      "A storage room with an item inside.");
    tile_modifier = Some Item;
  };
  {
    default_tile with
    tile_name = "Bat";
    tile_desc = "Lose 1 Might upon entry";
    tile_right = true;
    tile_left = true;
    enter_function = (fun (state: game_state) ->
        let plyr = curr_plyr state in
        Uifile.render_game state;
        change_stat plyr state ~-1 Might)
  };
  {
    default_tile with
    tile_name = "Shoe";
    tile_right = true;
    tile_left = true;
    enter_function = (fun state ->
        Uifile.render_game state;
        Uifile.render_popup_text "Entered Shoe" "Roll 1 die to lose Might");
    tile_modifier = Some Item;
  };
  {
    default_tile with
    tile_name = "Study";
    tile_left = true;
    tile_right = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Living";
    tile_name_abbrev = "Livn";
    tile_desc = "A living room";
    tile_up = true;
    tile_right = true;
  };
  {
    default_tile with
    tile_name = "Coat";
    tile_desc = "Lots of jackets in here";
    tile_right = true;
    tile_down = true;
  };
  {
    default_tile with
    tile_name = "Dinin";
    tile_desc = "A nice dining room";
    tile_right = true;
    tile_down = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Zoo";
    tile_desc = "How big is this room?";
    tile_right = true;
    tile_down = true;
    tile_left = true;
    tile_up = true;
  };
  {
    default_tile with
    tile_name = "Barn";
    tile_desc = "Animals go here";
    tile_right = true;
    tile_left = true;
  };
  { default_tile with
    tile_name = "Cave";
    tile_right = true;
    tile_left = true;
    enter_function = (fun (state: game_state) ->
        Uifile.render_popup_text "You are feeling weak..." "Lose 1 might";
        let pl = curr_plyr state in
        change_stat pl state ~-1 Might)
  };
  {
    default_tile with
    tile_name = "Cellar";
    tile_name_abbrev = "Celr";
    tile_right = true;
    tile_left = true;
    enter_function = (fun state ->
        Uifile.render_popup_text "You hear a strange noise and suddenly
...a racoon jumps at you"
          "Roll 1 die to Lose might";
        roll_action state Might 1 Change)
  };
  {
    default_tile with
    tile_name = "Lair";
    tile_right = true;
    tile_left = true;
    enter_function = (fun state ->
        Uifile.render_game state;
        Uifile.render_popup_text "entered lair" "roll 2 dice to lose sanity";
        roll_action state Sanity 2 Change)
  };
  {
    default_tile with
    tile_name = "Marina";
    tile_name_abbrev = "Mari";
    tile_up = true;
    tile_down = true;
    enter_function = (fun state ->
        Uifile.render_popup_text "Who stole all the boats?" "roll 2 die.
If you roll greater than 2, lose 1 speed";
        roll_action state Speed 2 (Compare (2, 1)))
  };
  {
    default_tile with
    tile_name = "Cage";
    tile_modifier = Some (Omen 0);
    tile_down = true;
    tile_up = true;
    tile_left = true
  };
  {
    default_tile with
    tile_name = "Firepit";
    tile_name_abbrev = "Fire";
    tile_desc = (
      "A warm and relaxing fire burns slowly.\n\n"^
      "Once per game, ending your turn here grants +2 Sanity");
    tile_down = true;
    tile_up = true;
    tile_left = true;
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 2 Sanity
      )
  };
  {
    default_tile with
    tile_name = "Spa";
    tile_desc = (
      "Sit back and relax.\n\n"^
      "Once per game, ending your turn here grants +1 All stats");
    tile_down = true;
    tile_up = true;
    tile_left = true;
    end_function = (fun state ->
        let plyr = curr_plyr state in
        change_stat plyr state 1 Might;
        change_stat plyr state 1 Speed;
        change_stat plyr state 1 Sanity;
        change_stat plyr state 1 Knowledge;
      )
  };
  {
    default_tile with
    tile_name = "Foot";
    tile_desc = "End here and make a Speed roll to gain Knowledge";
    tile_up = true;
    tile_down = true;
    end_function = (fun state ->
        let roll = roll_by_stat (curr_plyr state) Speed in
        stat_changes
          "Your knowledge is maxed out"
          "Your knowledge increases thanks to the shoe"
          Knowledge
          roll
          state)
  };
  {
    default_tile with
    tile_name = "Bear";
    tile_desc = "Lose Sanity when exiting this room";
    tile_up = true;
    tile_left = true;
    tile_right = true;
    exit_function = (fun state ->
        Uifile.render_game state;
        Uifile.render_popup_text "Exiting bear" "Roll 1 dice to lose Sanity";
        roll_action state Sanity 1 Change)
  };
  {
    default_tile with
    tile_name = "Water";
    tile_desc = "Ending your turn here requires you to roll 2
dice to lose Knowledge";
    tile_up = true;
    tile_left = true;
    tile_right = true;
    end_function = (fun state ->
        Uifile.render_popup_text "Ended turn in Water" "Roll 2 dice to lose Knowledge";
        roll_action state Knowledge 2 Change)
  };
  {
    default_tile with
    tile_name = "Closet";
    tile_name_abbrev = "Clst";
    tile_desc = "When discovered, you get an item from this closet.
       Draw a card to choose one";
    tile_modifier = Some Item;
    tile_up = true;
    tile_left = true;
    tile_right = true;
  };
  {
    default_tile with
    tile_name = "Spook";
    tile_desc  = "This room contains an omen";
    tile_modifier = Some (Omen 0);
    tile_up = true;
    tile_left = true;
    tile_right = true;
  }
]

(* the default_card *)
let default_card : card = {
  card_name = "";
  card_desc = "";
  card_modifier = Event;
  weapon = false;
  damage = 0;
  range = 0;
  consumable = false;
  ownable = false;
  usable = false;
  draw_function = (fun x -> ());
  use_function = (fun x -> ());
  passive_function = (fun x -> ());
}

(* all cards that can be played *)
let all_cards : card list = [
  {
    default_card with
    card_name = "Weak juice";
    card_desc = "Makes you lose 2 might";
    consumable = true;
    ownable = true;
    usable = true;
    card_modifier = Item;
    use_function = (fun (state: game_state) ->
        stat_changes
          "You're Might cant get any lower!"
          "You use the Weak Juice and lose 2 might!"
          Might
          2
          state)
  };
  {
    default_card with
    card_name = "Pale ale";
    card_desc = "Drink this to and roll 1 die to lose knowledge";
    consumable = true;
    ownable = true;
    usable = true;
    card_modifier = Item;
    use_function = (fun (state: game_state) ->
        roll_action state Knowledge 3 Change)
  };
  {
    default_card with
    card_name = "Omen";
    card_modifier = Omen 3
  };
  {
    default_card with
    card_name = "Smart Hat";
    card_desc = "Use this item to instantly gain 1 Knowledge";
    card_modifier = Item;
    ownable = true;
    usable = true;
    consumable = false;
    use_function = (fun state ->
        stat_changes
          "You use the Smart Hat but cant get any smarter!"
          "You use the Smart Hat and gain 1 Knowledge!"
          Knowledge
          1
          state)
  };
  {
    default_card with
    card_name = "Raven";
    card_desc = ("Nevermore");
    card_modifier = Omen 3
  };
  {
    default_card with
    card_name = "Spinning top";
    card_desc = (
      "Is this a dream?\n"^
      "How would you know its a dream?\n"^
      "The top keeps spinning, maybe it's an Inception dream.\n"^
      "Theres only one way to find out.\n"^
      "THIS IS NOT REALITY...?");
    card_modifier = Omen 5
  };
  {
    default_card with
    card_name = "Big Rock";
    card_desc = "Throw at people to hurt them";
    card_modifier = Item;
    damage = 4;
    range = 4;
    consumable = true;
    weapon = true;
    ownable = true;
    usable = true;
    use_function = (fun gs -> ());
  };
  {
    default_card with
    card_name = "Cursed Dagger";
    card_desc = "An eerie dagger crafted for killing evil spirits";
    card_modifier = Item;
    damage = 3;
    range = 0;
    consumable = false;
    weapon = true;
    ownable = true;
    usable = true;
    use_function = (fun gs -> ());
  };
  {
    default_card with
    card_name = "Band aid";
    card_desc = "Gain two sanity";
    card_modifier = Item;
    damage = 4;
    range = 4;
    consumable = true;
    weapon = false;
    ownable = true;
    usable = true;
    use_function = (fun state ->
        stat_changes
          "You use the Band Aid but already have full sanity!"
          "You use the Band Aid and gain 2 Sanity!"
          Sanity
          2
          state);
  }
]

(* default characters, Any resemblance to actual persons, living or dead,
   is purely coincidental.*)
let default_character = {
  name = "Nate Foster";
  desc = (
    "My research uses ideas from programming \n"^
    "languages to solve problems in networking,\n"^
    "databases, and security. Some topics of\n"^
    "interest include semantics, type systems,\n"^
    "bidirectional languages, data\n"^
    "synchronization, and mechanized proof.\n"^
    "Recently I spend most of my time thinking\n"^
    "about network programming.\n \n" ^
    "Any resemblance to actual persons,\n" ^
    "living or dead, or writings by said \n " ^
    "persons, is purely coincidental"
  );
  sanity = [1;3;3;4;5;5;6;7];
  knowledge = [4;5;5;5;5;6;7;8];
  might = [2;2;3;4;5;5;6;6];
  speed = [2;2;4;4;5;5;6;6];
  sanity_initial = 2;
  knowledge_initial = 4;
  might_initial = 2;
  speed_initial = 5;
}

(* all characters that can be played in the game *)
let all_characters : character list = [
  default_character;
  {
    name = "Note Faster";
    desc = (
      "A distant cousin of Nate Foster who\n"^
      "trained his entire life to become the\n"^
      "fastest human being alive. He failed,\n"^
      "but he's still kinda fast, I guess."
    );
    sanity = [1;2;3;4;4;5;5;6];
    knowledge = [4;4;4;5;5;6;7;7];
    might = [1;2;3;4;4;5;5;6];
    speed = [4;4;4;5;6;7;8;9];
    sanity_initial = 2;
    knowledge_initial = 2;
    might_initial = 2;
    speed_initial = 7;
  };
  {
    default_character with
    name = "Not Foster";
    desc = (
      "A failed top-secret government\n"^
      "experiment to clone Nate Foster in\n"^
      "order to create a superhuman army\n"^
      "and wipe out overseas enemies."
    );
    speed_initial = 5;
    might_initial = 7;
    speed = [1;1;1;1;1;1;1;9];
    knowledge = [1;1;1;1;1;1;1;9];
    sanity = [1;1;1;1;1;1;1;9];
    might = [1;1;1;1;1;1;1;9];
  };
  {
    name = "New Feaster";
    desc = (
      "Champion of the world-wide hotdog-\n"^
      "eating contest for 10 years in a row.\n"^
      "Dangerously out-of-shape."
    );
    sanity = [2;2;3;3;4;5;5;6];
    knowledge = [1;2;3;4;4;5;5;6];
    might = [2;2;3;4;5;5;6;7];
    speed = [1;2;2;2;3;3;3;4];
    sanity_initial = 2;
    knowledge_initial = 3;
    might_initial = 4;
    speed_initial = 3;
  };
  {
    name = "Need Features";
    desc = (
      "TODO: \n"^
      "Write description for Need Features."
    );
    sanity = [2;2;2;2;3;3;3;3];
    knowledge = [2;2;2;2;3;3;3;3];
    might = [2;2;2;2;3;3;3;3];
    speed = [3;3;3;4;4;4;4;4];
    sanity_initial = 4;
    knowledge_initial = 4;
    might_initial = 3;
    speed_initial = 5;
  };
  {
    name = "Neat Feeter";
    desc = (
      "Famous salsa dancer from Spain. \n"^
      "Established peace between warring\n"^
      "sovereign nations through the \n"^
      "power of dance."
    );
    sanity = [2;2;2;2;3;3;3;3];
    knowledge = [2;2;2;2;3;3;3;3];
    might = [2;2;2;2;3;3;3;3];
    speed = [3;3;3;4;4;4;4;4];
    sanity_initial = 4;
    knowledge_initial = 4;
    might_initial = 3;
    speed_initial = 5;
  };
  {
    default_character with
    name = "Newt Fester";
    speed_initial = 5;
    sanity = [1;2;2;3;3;7;8;9];
  };
  {
    name = "Greg";
    desc = (
      "Greg"
    );
    sanity = [1;2;3;4;5;6;6;7];
    knowledge = [2;3;4;4;5;6;6;7];
    might = [2;3;3;3;4;5;6;7];
    speed = [3;4;5;6;6;6;7;7];
    sanity_initial = 2;
    knowledge_initial = 3;
    might_initial = 3;
    speed_initial = 5;
  };
]

(* help text for the game *)
let help_text = "Betrayal is a four person keyboard based board game. \n" ^
                "Instructions for each turn are displayed in this box. \n"^
                "Walk through the house using 'w' 'a' 's' 'd' keys \n" ^
                "Take turns exploring the house, but beware of the many \n" ^
                "monsters and traps around you. \n " ^
                "Some rooms will contain Omens when you enter. The more \n" ^
                "omens discovered, the likely it is that a haunt will be \n" ^
                "triggered. You might be suprised by who you're true enemies are."
