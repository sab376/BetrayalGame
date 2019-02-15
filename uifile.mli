open Data
open Graphics

(* Initializes the graphics system be calling create_graph from Graphics *)
val initialize_render : unit -> unit

(* A more visible blue than Graphics.blue *)
val blue : int

(* renders the game based on the current game state *)
val render_game : game_state -> unit

(* [render_popup_text title body] renders a standardized popup at the top of the
   screen with a corresponding title and body.
   Allow newlines.
   If the title is empty, the popup will only occupy the size of the title.*)
val render_popup_text : string -> string -> unit

(*draw_dice [x y is] draws set of dice starting at [x] and [y] with values [is]*)
val draw_dice : int -> int -> int list -> unit

(*[draw_dice_with_string px py s (is: int list)] draws [s] centered at [px] [py]
  followed by [is]. *)
val draw_dice_with_string : int -> int -> string -> int list -> unit

(* [draw_dice_animated px py n roll] is animation of [n] dice rolling at [px]
   and [py]. until user clicks and ends on [roll]
   Requires: length of [roll] = n+1 *)
val draw_dice_animated: int-> int-> int -> int list -> unit

(* [animated_draw_dice_with_string px py n roll s] is animation of [n] dice rolling at [px]
   and [py]. Animation goes until user clicks and ends with string printed next to [roll]
   Requires: length of [roll] = n+1 *)
val animated_draw_dice_with_string : int -> int -> int -> string -> int list -> unit

(* [draw_centered_text pt str] draws [str] centered at [pt] *)
val draw_centered_text : point -> string -> unit

(* the size of the screen at which the game will be played *)
val screen_size : point

(* list of colors for each player *)
val player_colors : int list

(* ButtonHandler is an abstract type to be used for multi-choice selections
   via mouse click.
   ButtonHandlers are created by starting with empty and calling the add function
   to build more buttons.
   The only mutable part of ButtonHandlers are its text and enabled properties
   for each button, which can be set with set_text and set_enabled respectively. *)
module type ButtonHandlerSig = sig
  type 'a t

(* The empty button handler, contains no buttons
   this is where you declare the size and center of your button set *)
  val empty : point -> point -> int -> 'a t

(* Adds a button onto an existing button handler, and return s anew button handler *)
  val add : 'a -> string -> int -> 'a t -> 'a t

(* Returns the index of the first button with the property 'a assigned to it
   Throws Not_Found if no such button *)
  val fetch_index : int -> 'a t -> 'a

(* Sets the enabled bool of the first button with property 'a assigned to
   it. A button with enabled=false will be greyed out and will not register clicks *)
  val set_enabled : 'a -> bool -> 'a t -> unit

(* Sets the text of the first button with property 'a assigned to it. *)
  val set_text : 'a -> string -> 'a t -> unit

(* Set_enabled, but takes in the index of the button instead of the 'a property *)
  val set_enabled_index : int -> bool -> 'a t -> unit

(* Returns the 'a property of the button in which the mouse click falls as
   an option, or None if no button was clicked *)
  val mouse_input : (int * int) -> 'a t -> 'a option

(* Draws all the buttons of a ButtonHandler on the screen *)
  val render : 'a t -> unit
end

module ButtonHandler : ButtonHandlerSig

(* Renders up to three characters on the screen, with their names, stats, and
   descriptions. Aligned with char_buttons *)
val render_char_selection : character list -> game_state -> unit

(* Creates char_buttons ButtonHandler to be used with character selection *)
val create_char_buttons : unit -> int ButtonHandler.t
