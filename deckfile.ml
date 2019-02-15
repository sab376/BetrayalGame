open Data

module type Deckable = sig
  type t
  type attribute

  val matches : t -> attribute -> bool
end

module type DeckModule = sig
  type t
  type single
  type attribute

  (* returns tuple of remainder of deck without chosen single,
     * first thing in deck with correct attribute and
     * Raises: Out_of_singles if no applicable single *)
  val next : t -> attribute -> t * single

  (* creates a new randomly ordered Deck from inputted single list *)
  val init: single list -> t
end

(* DeckMaker takes in a Deckable and returns a corresponding Deck *)
module DeckMaker =
  functor (In : Deckable) ->
  struct
    type t = In.t list
    type single = In.t
    type attribute = In.attribute

    let init lst =
      List.sort (fun a b -> (Random.int 2) * 2 -1) lst

    let next lst att =
      let rec go_through lst att disc =
          match lst with
          | [] -> failwith "Out_of_singles"
          | h::t when In.matches h att -> (t @ disc,h)
          | h::t-> go_through t att (h::disc)
      in
      go_through lst att []
  end

let _ = Random.self_init ()

(*deckable type to build tile list*)
module TileDeckable:(Deckable with type t = tile and type attribute = area) = struct
  type t = tile
  type attribute = area
  let matches (tile:t) floor =
    List.mem floor tile.areas
end

module TileDeck = DeckMaker(TileDeckable)

let tiles = TileDeck.init (Staticdata.all_tiles)

module CardDeckable:(Deckable with type t = card and type attribute = modifier) = struct
  type t = card
  type attribute = modifier
  let matches (card:t) (m:attribute) =
    match (card.card_modifier, m) with
    | (Event, Event) -> true
    | (Item, Item) -> true
    | (Omen _, Omen _) -> true
    | _ -> false
end

module CardDeck = DeckMaker(CardDeckable)

let cards = TileDeck.init (Staticdata.all_cards)
