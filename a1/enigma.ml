(** @author TODO Name (netid) *)

(** [index c] is the 0-based index of [c] in the alphabet.
    Requires: [c] is an uppercase letter in A..Z. *)
let index (c:char) : int =
  Char.code c - 65


(** [map_r_to_l wiring top_letter input_pos] is the left-hand output position
    at which current would appear when current enters at right-hand input
    position [input_pos] to a rotor whose wiring specification is given by
    [wiring].  The orientation of the rotor is given by [top_letter], 
    which is the top letter appearing to the operator in the rotor's 
    present orientation.
    Requires: 
    	[wiring] is a valid wiring specification, 
    	[top_letter] is in A..Z, and 
    	[input_pos] is in 0..25. *)
let map_r_to_l (wiring:string) (top_letter:char) (input_pos:int) : int =

  let offset = index top_letter in

  let rhc =  if input_pos + offset < 26 then input_pos + offset 
  else input_pos + offset - 26 in

  let cipher_number = String.get wiring rhc in

  let lhc = index cipher_number in

  let output = if lhc - offset > -1 then lhc - offset
  else lhc - offset + 26 in

  output

(** [map_l_to_r] computes the same function as [map_r_to_l], except 
    for current flowing left to right. *)
let map_l_to_r (wiring:string) (top_letter:char) (input_pos:int) : int =

  let plaintext = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let offset = index top_letter in

  let lhc = if input_pos + offset < 26 then input_pos + offset 
  else input_pos + offset - 26 in

  let input_letter = String.get plaintext lhc in

  let n = String.index wiring input_letter in

  let w = String.get plaintext n in

  let rhc = index w in

  let output = if rhc - offset > -1 then rhc - offset
  else rhc - offset + 26 in

  output

(** [map_refl wiring input_pos] is the output position at which current would 
    appear when current enters at input position [input_pos] to a reflector 
    whose wiring specification is given by [wiring].
    Requires: 
    	[wiring] is a valid reflector specification, and 
      [input_pos] is in 0..25. *)
let map_refl (wiring:string) (input_pos:int) : int =

  map_r_to_l wiring 'A' input_pos

(** [map_plug plugs c] is the letter to which [c] is transformed
    by the plugboard [plugs].
    Requires:
      [plugs] is a valid plugboard, and
      [c] is in A..Z. *)
let rec map_plug (plugs:(char*char) list) (c:char) =

  let rec plug_match (plug_list:(char*char) list) (input:char)= 
    match plug_list with
      | [] -> input
      | (x,y)::t -> if input = x then y
                else if input = y then x
                else plug_match t input
  in

  plug_match plugs c

(** [rotor] represents an Enigma rotor. *)
type rotor = {
  (** A valid rotor wiring specification. *)
  wiring : string;
  (** The turnover of the rotor, which must be an uppercase letter.
      This field will not be used in the assignment until you 
      implement stepping in the excellent scope. *)
  turnover : char;
}

(** [oriented_rotor] represents a rotor that is installed on the spindle hence 
    has a top letter. *)
type oriented_rotor = {
  (** The rotor. *)
  rotor : rotor;
  (** The top letter showing on the rotor. *)
  top_letter : char;
}

(** [config] represents the configuration of an Enigma machine. *)
type config = {
  (** A valid reflector wiring specification. *)
  refl : string;
  (** The rotors as they are installed on the spindle from left to right.
      There may be any number of elements in this list: 0, 1, 2, 3, 4, 5, etc.
      The order of elements in list represents the order in which the rotors
      are installed on the spindle, **from left to right**. So, the head of the
      list is the leftmost rotor on the spindle, and the last element of the
      list is the rightmost rotor on the spindle.  *)
  rotors : oriented_rotor list;
  (** A valid plugboard. The order of characters in the pairs does not matter,
      and the order of pairs in the list does not matter. *)
  plugboard : (char*char) list;
}

(** [cipher_char config c] is the letter to which the Enigma machine 
    ciphers input [c] when it is in configuration [config].
    Requires:
      [config] is a valid configuration, and
      [c] is in A..Z. *)
let cipher_char (config:config) (c:char) : char =

  let rev_rotors = List.rev config.rotors in

  (** [map_rotors_r_to_l] is the output position from traversing the series of 
  rotors [config.rotors] right-to-left given the input letter [c]. *)
  let rec map_rotors_r_to_l (rotors : (oriented_rotor) list) (input : int) =
    match rev_rotors with
      | [] -> input
      | h::t -> map_rotors_r_to_l t (map_r_to_l h.rotor.wiring h.top_letter input)
  in

  (** [map_rotors_l_to_r] is the output position from traversing the series of 
  rotors [config.rotors] left-to-right given the input letter [c]. *)
  let rec map_rotors_l_to_r (rotors : (oriented_rotor) list) (input : int) =
    match config.rotors with 
      | [] -> input
      | h::t -> map_rotors_l_to_r t (map_r_to_l h.rotor.wiring h.top_letter input)
  in

  c 
  |> map_plug config.plugboard 
  |> index 
  |> map_rotors_r_to_l config.rotors 
  |> map_refl config.refl
  |> map_rotors_l_to_r config.rotors
  |> (+) 65
  |> Char.chr
  |> map_plug config.plugboard


(** [step config] is the new configuration to which the Enigma machine 
    transitions when it steps beginning in configuration [config].
    Requires: [config] is a valid configuration. *)
let step (config:config) : config =
  failwith "Unimplemented"

(** [cipher config s] is the string to which [s] enciphers
    when the Enigma machine begins in configuration [config].
    Requires:
      [config] is a valid configuration, and
      [s] contains only uppercase letters. *)
let rec cipher (config:config) (s:string) : string =
  failwith "Unimplemented"

(* TODO: set the value below *)
let hours_worked = 7
