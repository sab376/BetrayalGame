open Data

(* Defines a type that can be made into a deck *)
module type Deckable = sig
  type t
      (*defines attribute of deckable as in Omen, Event, or Item *)
  type attribute

(*[matches t att] is true if attribute of [t] is same type as [att], false otherwise*)
  val matches : t -> attribute -> bool
end

  (* Defines a return type of DeckMaker *)
module type DeckModule = sig
  type t
  (* list of items in deck *)
  type single
  type attribute

  (* returns tuple of remainder of deck without chosen single,
     * first single in deck with correct attribute and
     * Raises: Out_of_singles if no applicable single *)
  val next : t -> attribute -> t * single

  (* creates a new randomly ordered Deck from inputted single list *)
  val init: single list -> t
end

(* DeckMaker takes in a Deckable and returns a corresponding Deck *)
module DeckMaker (In : Deckable) : DeckModule

(* TileDeck is a deck of tiles, attribute tracks where each tile may be
   placed: Ground | Basement | Attic *)
module TileDeck : (DeckModule with type single = tile and type t = tile list
                                                      and type attribute = area)
(* list of tiles *)
val tiles : TileDeck.t

(* CardDeck is a deck of cards, attribute tracks the type of each card:
   Omen | Item | Event *)
module CardDeck : (DeckModule with type single = card
                               and type t = card list and type attribute = modifier)
(* list of cards *)
val cards : CardDeck.t
