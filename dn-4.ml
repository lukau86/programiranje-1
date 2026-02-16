(*----------------------------------------------------------------------------*
  # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ## Slovarji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali iskalna drevesa in implementacijo 
  AVL-dreves za predstavitev množic. V tej nalogi morate s pomočjo AVL-dreves 
  implementirati `slovar`, ki preslika ključe tipa `'k` v vrednosti tipa `'v`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ### Stroga ureditev

  Za predstavitev slovarja potrebujemo strogo ureditev na tipu ključev.
  Najprej definiramo tip `ureditev`, ki predstavlja možne izide
  primerjave dveh elementov, nato pa še modul `UREJEN_TIP`, s katerim
  lahko primerjamo abstraktne elemente.


[*----------------------------------------------------------------------------*)

type ureditev = Less | Equal | Greater

module type UREJEN_TIP = sig
  type t

  val primerjaj : t -> t -> ureditev
end

module INT_UREJEN_TIP : UREJEN_TIP with type t = int = struct
  type t = int

  let primerjaj x y = if x < y then Less else if x > y then Greater else Equal
end

(*----------------------------------------------------------------------------*
  Sestavite modul `STRING_UREJEN_TIP`, ki implementira `UREJEN_TIP` za tip
  `string`.
[*----------------------------------------------------------------------------*)

module STRING_UREJEN_TIP : UREJEN_TIP with type t = string = struct
  type t = string

  let primerjaj x y = if x < y then Less else if x > y then Greater else Equal
end;;

STRING_UREJEN_TIP.primerjaj "abc" "abd"
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  Za poljuben tip lahko definiramo `razširitev` z najmanjšim in največjim
  elementom. Sestavite parametriziran modul `RAZSIRJEN_UREJEN_TIP`, ki
  sprejme modul, ki implementira signaturo `UREJEN_TIP`, in vrne modul, ki
  implementira signaturo `UREJEN_TIP` za razširjeni tip.
[*----------------------------------------------------------------------------*)

type 'a razsiritev = MinInf | PlusInf | Value of 'a

module RAZSIRJEN_UREJEN_TIP (U : UREJEN_TIP) :
  UREJEN_TIP with type t = U.t razsiritev = struct
  type t = U.t razsiritev

  let primerjaj x y = match (x, y) with
  | (MinInf, MinInf) -> Equal
  | (PlusInf, PlusInf) -> Equal
  | (MinInf, PlusInf) -> Less
  | (PlusInf, MinInf) -> Greater
  | (MinInf, Value y) -> Less
  | (PlusInf, Value y) -> Greater
  | (Value x, MinInf) -> Greater
  | (Value x, PlusInf) -> Less
  | (Value x, Value y) -> U.primerjaj x y
end

module LIFTED_INT_UREJEN_TIP = RAZSIRJEN_UREJEN_TIP (INT_UREJEN_TIP);;

LIFTED_INT_UREJEN_TIP.primerjaj MinInf (Value 3)
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  ### AVLSlovar

  Sestavite parametriziran modul `MAKE_SLOVAR`, ki sprejme modul, ki
  implementira `UREJEN_TIP`, in vrne modul s signaturo `SLOVAR`. Vaš slovar
  naj bo implementiran z AVL-drevesi, tako da je vstavljanje in iskanje v
  slovarju v času `O(log n)`.
[*----------------------------------------------------------------------------*)

module type SLOVAR = sig
  type kljuc
  type 'a t

  val prazen : 'a t
  (** Vrne prazen slovar. *)
  val dodaj : kljuc -> 'a -> 'a t -> 'a t
  (** Doda nov par `kljuc`, `vrednost` v slovar. Če ključ v slovarju že obstaja, 
      se njegova vrednost posodobi. *)
  val popravi : kljuc -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Popravi vrednost pod ključem `kljuc` s funkcijo `f`. Če ključ v slovarju
      ne obstaja, se pokliče `f None`, sicer `f (Some vrednost)`. Če je rezultat
      klica `f` enak `None`, se par odstrani iz slovarja, če je rezultat klica 
      `Some v`, se pod ključ `kljuc` zapiše vrednost `v`.*)
  val odstrani : kljuc -> 'a t -> 'a t
  (** Odstrani par s ključem `kljuc` iz slovarja. Če ključa v slovarju ni, naj 
      funkcija vrne prvotni slovar in ne sproži napake. *)
  val velikost : 'a t -> int
  (** Vrne število elementov v slovarju. *)
  val kljuci : 'a t -> kljuc list
  (** Našteje ključe v slovarju v enakem vrstnem redu kot to določa urejenost. *)
  val vrednosti : 'a t -> 'a list
  (** Našteje vrednosti v slovarju v enakem vrstnem redu kot to določa urejenost
      pripadajočih ključev. *)
  val najmanjsi_opt : 'a t -> (kljuc * 'a) option
  (** Vrne najmanjši ključ v slovarju ali `None`, če je slovar prazen. *)
  val najvecji_opt : 'a t -> (kljuc * 'a) option
  (** Vrne največji ključ v slovarju ali `None`, če je slovar prazen. *)
  val poisci_opt : kljuc -> 'a t -> 'a option
  (** Poišče vrednost pod ključem `kljuc`. Če ključ v slovarju ne obstaja,
      vrne `None`. *)
  val iter : (kljuc -> 'a -> unit) -> 'a t -> unit
  (** Izvede funkcijo za vsak par ključ, vrednost v slovarju v enakem vrstnem 
      redu kot ga določa urejenost. *)
  val zlozi : (kljuc -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Zloži slovar z dano funkcijo in začetno vrednostjo. Elementi se obdelajo
      v enakem vrstnem redu kot ga določa urejenost.
  
      Specifično za
      `zlozi f slovar acc = f k_n v_n (... (f k_2 v_2 (f k_1 v_1 acc))...)`
      , kjer so `(k_1, v_1), ..., (k_n, v_n)` pari ključ, vrednost v slovarju 
      urejeni po ključih.
  *)
  val preslikaj : ('a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo. *)
  val preslikaji : (kljuc -> 'a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo, ki poleg vrednosti dobi še
      pripadajoči ključ. *)
  val vsebuje : kljuc -> 'a t -> bool
  (** Preveri, ali slovar vsebuje podan ključ. *)
  val za_vse : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali za vse pare ključ, vrednost v slovarju velja podan pogoj. *)
  val obstaja : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali obstaja vsaj en par ključ, vrednost v slovarju, ki izpolnjuje
      podan pogoj. *)
  val v_seznam : 'a t -> (kljuc * 'a) list
  (** Pretvori slovar v seznam parov ključ, vrednost v enakem vrstnem redu kot
      to določa urejenost. *)
  val iz_seznama : (kljuc * 'a) list -> 'a t
  (** Ustvari slovar iz seznama parov ključ, vrednost. Če se ključi v seznamu
      ponavljajo, naj enak ključ obdrži zadnjo vrednost. *)
end


module MAKE_SLOVAR (U : UREJEN_TIP) : SLOVAR with type kljuc = U.t = struct
  type kljuc = U.t
  type 'a t = Empty | Node of { levo : 'a t; desno : 'a t; k : kljuc; v : 'a; h : int; s : int }

  let height = function
    | Empty -> 0
    | Node n -> n.h

  let size = function
    | Empty -> 0
    | Node n -> n.s

  let nov_node l d k v = Node { levo = l; desno = d; k = k; v = v; h = 1 + max (height l) (height d); s = 1 + (size l) + (size d) }

  let rot_ll = function
    | Node { levo = l; desno = Node d; k; v; _ } -> nov_node (nov_node l d.levo k v) d.desno d.k d.v
    | _ -> assert false

  let rot_rr = function 
    | Node { levo = Node l; desno = d; k; v; _ } -> nov_node l.levo (nov_node l.desno d k v) l.k l.v
    | _ -> assert false

  let rot_lr = function
    | Node { levo = l; desno = Node { levo = Node l1; desno = d1; k = k1; v = v1; _ }; k; v; _ } -> nov_node (nov_node l l1.levo k v) (nov_node l1.desno d1 k1 v1) l1.k l1.v
    | _ -> assert false

  let rot_rl = function
    | Node { levo = Node { levo = l1; desno = Node d1; k = k1; v = v1; _ }; desno = d; k; v; _ } -> nov_node (nov_node l1 d1.levo k1 v1) (nov_node d1.desno d k v) d1.k d1.v
    | _ -> assert false

  let uravnotezi = function (* predpostavljamo, da so nizji nivoji ze uravnotezeni *)
    | Node n ->
      let dh = height n.levo - height n.desno in
      if dh > 1 then
        match n.levo with
        | Node n1 ->
          if height n1.levo > height n1.desno then
            rot_rr (Node n)
          else
            rot_rl (Node n)
        | Empty -> assert false
      else if dh < -1 then
        match n.desno with
        | Node n1 ->
          if height n1.levo < height n1.desno then
            rot_ll (Node n)
          else
            rot_lr (Node n)
        | Empty -> assert false
      else
        Node n
    | Empty -> Empty

  let prazen : 'a t = Empty

  let rec dodaj k v n = match n with
    | Empty -> nov_node Empty Empty k v
    | Node n ->
      match U.primerjaj k n.k with
      | Equal -> Node { n with v = v }
      | Less -> uravnotezi (nov_node (dodaj k v n.levo) n.desno n.k n.v)
      | Greater -> uravnotezi (nov_node n.levo (dodaj k v n.desno) n.k n.v)

  let rec odstrani k n = match n with
    | Empty -> Empty
    | Node n -> 
      match U.primerjaj k n.k with
      | Less -> uravnotezi (nov_node (odstrani k n.levo) n.desno n.k n.v)
      | Greater -> uravnotezi (nov_node n.levo (odstrani k n.desno) n.k n.v)
      | Equal -> 
        match (n.levo, n.desno) with
        | Empty, _ -> n.desno
        | _, Empty -> n.levo
        | _ -> 
          let rec aux = function
            | Empty -> (None, Empty)
            | Node n -> 
              match n.levo with
              | Empty -> (Some (n.k, n.v), n.desno)
              | _ ->
                let (m, levo) = aux n.levo in
                (m, uravnotezi (nov_node levo n.desno n.k n.v))
          in
          match aux n.desno with
          | (Some (kmin, vmin), desno) -> uravnotezi (nov_node n.levo desno kmin vmin)
          | (None, _) -> assert false

  let velikost n = size n

  let rec kljuci n = match n with
    | Empty -> []
    | Node n -> kljuci n.levo @ [n.k] @ kljuci n.desno

  let rec vrednosti n = match n with
    | Empty -> []
    | Node n -> vrednosti n.levo @ [n.v] @ vrednosti n.desno

  let rec najmanjsi_opt n = match n with
    | Empty -> None
    | Node n -> match n.levo with
      | Empty -> Some (n.k, n.v)
      | _ -> najmanjsi_opt n.levo
      
  let rec najvecji_opt n = match n with
    | Empty -> None
    | Node n -> match n.desno with
      | Empty -> Some (n.k, n.v)
      | _ -> najvecji_opt n.desno
      
  let rec poisci_opt k n = match n with
    | Empty -> None
    | Node n -> 
      match U.primerjaj k n.k with
      | Equal -> Some n.v
      | Greater -> poisci_opt k n.desno
      | Less -> poisci_opt k n.levo

  let popravi k f n = match f (poisci_opt k n) with
    | None -> odstrani k n
    | Some v -> dodaj k v n

  let iter f n =
    let rec aux = function
      | Empty -> ()
      | Node n -> aux n.levo; f n.k n.v; aux n.desno
    in
    aux n

  let zlozi f n acc = 
    let rec aux acc = function
      | Empty -> acc
      | Node n -> aux (f n.k n.v (aux acc n.levo)) n.desno
    in
    aux acc n
  let rec preslikaj f n = match n with
    | Empty -> Empty
    | Node n -> nov_node (preslikaj f n.levo) (preslikaj f n.desno) n.k (f n.v)

  let rec preslikaji f n = match n with
    | Empty -> Empty
    | Node n -> nov_node (preslikaji f n.levo) (preslikaji f n.desno) n.k (f n.k n.v)

  let vsebuje k n = match poisci_opt k n with
    | None -> false
    | Some _ -> true
  let rec za_vse f n = match n with
    | Empty -> true
    | Node n -> f n.k n.v && (za_vse f n.levo) && (za_vse f n.desno)
  let rec obstaja f n = match n with
    | Empty -> false
    | Node n -> f n.k n.v || (obstaja f n.levo) || (obstaja f n.desno)
  let v_seznam n = List.combine (kljuci n) (vrednosti n)
  let rec iz_seznama l = match l with
    | [] -> Empty
    | (k, v) :: t -> dodaj k v (iz_seznama t)
end

module SLOVAR_NIZ = MAKE_SLOVAR (STRING_UREJEN_TIP)

let slovar =
  SLOVAR_NIZ.iz_seznama
    [ ("jabolko", "apple"); ("banana", "banana"); ("cesnja", " cherry") ]
  |> SLOVAR_NIZ.dodaj "datelj" "date"
  |> SLOVAR_NIZ.odstrani "banana"
  |> SLOVAR_NIZ.popravi "cesnja" (function
       | None -> Some "cherry"
       | Some v -> Some ("sour " ^ v))
  |> SLOVAR_NIZ.preslikaj String.length
  |> SLOVAR_NIZ.v_seznam

(*----------------------------------------------------------------------------*
  ## Turingovi stroji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali Turingove stroje. Pred vami je
  neučinkovito implementiran Turingov stroj. Vaša naloga je, da implementacijo
  s pomočjo slovarjev izboljšate tako, da bo deloval učinkoviteje.
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

module Tape : TAPE = struct
  type t = { left : char list; right : char list }

  let make str = { left = []; right = str |> String.to_seq |> List.of_seq }

  let print { left; right } =
    List.rev_append left right |> List.to_seq |> String.of_seq |> print_endline;
    print_endline (String.make (List.length left) ' ' ^ "^")

  let read { right } = match right with [] -> ' ' | chr :: _ -> chr

  let move dir { left; right } =
    match (dir, left, right) with
    | Left, ' ' :: left, [] -> { left; right }
    | Left, chr :: left, right -> { left; right = chr :: right }
    | Left, [], right -> { left = []; right = ' ' :: right }
    | Right, [], ' ' :: right -> { left; right }
    | Right, left, chr :: right -> { left = chr :: left; right }
    | Right, left, [] -> { left = ' ' :: left; right = [] }

  let write chr { left; right } =
    match right with
    | [] when chr = ' ' -> { left; right }
    | [] -> { left; right = [ chr ] }
    | _ :: right -> { left; right = chr :: right }
end

let primer_trak =
  Tape.(
    make "ABCDE" |> move Left |> move Left |> move Right |> move Right
    |> move Right |> move Right |> write '!' |> print)

module type MACHINE = sig
  type t

  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
  val run : t -> state -> unit
  val speed_run : t -> state -> unit
end

module MachineNeucinkovito : MACHINE = struct
  type t = {
    initial : state;
    transitions : (state * char * state * char * direction) list;
  }

  let make initial _states = { initial; transitions = [] }
  let initial { initial } = initial

  let add_transition st chr st' chr' dir tm =
    { tm with transitions = (st, chr, st', chr', dir) :: tm.transitions }

  let step tm st tape =
    let chr = Tape.read tape in
    match
      List.find_opt
        (fun (st', chr', _, _, _) -> st = st' && chr = chr')
        tm.transitions
    with
    | None -> None
    | Some (_, _, st', chr', dir) ->
        Some (st', tape |> Tape.write chr' |> Tape.move dir)

  let run tm str =
    let rec step' (st, tape) =
      (Tape.print tape;
      print_endline st;
      match step tm st tape with
      | None -> ()
      | Some config' -> step' config')
    in
    step' (initial tm, Tape.make str)

  let speed_run tm str =
    let rec step' (st, tape) =
      match step tm st tape with
      | None -> Tape.print tape
      | Some config' -> step' config'
    in
    step' (initial tm, Tape.make str)
end

(*----------------------------------------------------------------------------*
  Sestavite modul `MachineUcinkovito`, ki učinkovito implementira signaturo
  `MACHINE` z uporabo slovarja, ki ste ga implementirali pred tem. Na kratko
  analizirajte časovno zahtevnost operacij `add_transition` in `step` v
  primerjavi z neučinkovito implementacijo.

  Namig:  
  Za dodatne točke je časovna zahtevnost iskanja prehoda v funkciji
  `speed_run` z nekaj preprocesiranja konstantna.
[*----------------------------------------------------------------------------*)

module PREHOD_UREJEN_TIP : UREJEN_TIP with type t = (state * char) = struct
  type t = state * char

  let primerjaj (x,c) (y,d) = if x < y then Less else if x > y then Greater else if c < d then Less else if c > d then Greater else Equal
end;;

module SLOVAR_PREHOD = MAKE_SLOVAR(PREHOD_UREJEN_TIP)

module MachineUcinkovito : MACHINE = struct
  type t = {
    initial : state;
    transitions : (state * char * direction) SLOVAR_PREHOD.t;
  }
  
  let make initial states = { initial = initial; transitions = SLOVAR_PREHOD.prazen }
  let initial { initial } = initial

  (*ta funkcija je O(log n), kjer je n število prehodov; nekoliko počasneje od neucinkovite mašine*)
  let add_transition st chr st' chr' dir tm = { tm with transitions = tm.transitions |> SLOVAR_PREHOD.dodaj (st, chr) (st', chr', dir) }
  (*ta funkcija je O(log n), kjer je n število prehodov; mnogo hitreje od neucinkovite mašine*)
  let step tm st tape =
    let chr = Tape.read tape in
    match
      SLOVAR_PREHOD.poisci_opt (st, chr) tm.transitions
    with
    | None -> None
    | Some (st', chr', dir) ->
        Some (st', tape |> Tape.write chr' |> Tape.move dir)
  let run tm str =
    let rec step' (st, tape) =
      (Tape.print tape;
      print_endline st;
      match step tm st tape with
      | None -> ()
      | Some config' -> step' config')
    in
    step' (initial tm, Tape.make str)
  let speed_run tm str =
    let rec step' (st, tape) =
      match step tm st tape with
      | None -> Tape.print tape
      | Some config' -> step' config'
    in
    step' (initial tm, Tape.make str)
end


(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhodnem nizu prepozna palindrom (iz `0` in
  `1`). Če je na vhodnem nizu palindrom, naj na koncu na traku zapiše `1`,
  sicer `0`.
[*----------------------------------------------------------------------------*)

let palindrom_stroj : MachineUcinkovito.t = MachineUcinkovito.(
  make "start_good" []
  |> add_transition "start_good" '0' "r_0_good" ' ' Right
  |> add_transition "r_0_good" '0' "r_0_good" '0' Right
  |> add_transition "r_0_good" '1' "r_0_good" '1' Right
  |> add_transition "r_0_good" ' ' "l_0_good" ' ' Left
  |> add_transition "l_0_good" '0' "end_good" ' ' Left
  |> add_transition "l_0_good" '1' "end_bad" ' ' Left
  |> add_transition "start_bad" '0' "r_0_bad" ' ' Right
  |> add_transition "r_0_bad" '0' "r_0_bad" '0' Right
  |> add_transition "r_0_bad" '1' "r_0_bad" '1' Right
  |> add_transition "r_0_bad" ' ' "l_0_bad" ' ' Left
  |> add_transition "l_0_bad" '0' "end_bad" ' ' Left
  |> add_transition "l_0_bad" '1' "end_bad" ' ' Left

  |> add_transition "start_good" '1' "r_1_good" ' ' Right
  |> add_transition "r_1_good" '0' "r_1_good" '0' Right
  |> add_transition "r_1_good" '1' "r_1_good" '1' Right
  |> add_transition "r_1_good" ' ' "l_1_good" ' ' Left
  |> add_transition "l_1_good" '1' "end_good" ' ' Left
  |> add_transition "l_1_good" '0' "end_bad" ' ' Left
  |> add_transition "start_bad" '1' "r_1_bad" ' ' Right
  |> add_transition "r_1_bad" '0' "r_1_bad" '0' Right
  |> add_transition "r_1_bad" '1' "r_1_bad" '1' Right
  |> add_transition "r_1_bad" ' ' "l_1_bad" ' ' Left
  |> add_transition "l_1_bad" '1' "end_bad" ' ' Left
  |> add_transition "l_1_bad" '0' "end_bad" ' ' Left

  |> add_transition "end_good" '0' "end_good" '0' Left
  |> add_transition "end_good" '1' "end_good" '1' Left
  |> add_transition "end_good" ' ' "start_good" ' ' Right

  |> add_transition "end_bad" '0' "end_bad" '0' Left
  |> add_transition "end_bad" '1' "end_bad" '1' Left
  |> add_transition "end_bad" ' ' "start_bad" ' ' Right

  |> add_transition "l_0_good" ' ' "quit" '1' Right
  |> add_transition "l_1_good" ' ' "quit" '1' Right
  |> add_transition "start_good" ' ' "quit" '1' Right
  |> add_transition "l_0_bad" ' ' "quit" '0' Right
  |> add_transition "l_1_bad" ' ' "quit" '0' Right
  |> add_transition "start_bad" ' ' "quit" '0' Right
)

let je_palindrom s = MachineUcinkovito.speed_run palindrom_stroj s

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhod sprejme niz `n` enic in na koncu na
  traku zapiše `n^2` enic.
[*----------------------------------------------------------------------------*)

let kvadrat_stroj : MachineUcinkovito.t = MachineUcinkovito.(
  make "start" []
  |> add_transition "start" '1' "make_border" '1' Left
  |> add_transition "make_border" ' ' "make_end" 'A' Right
  
  |> add_transition "make_end" '1' "make_end" '1' Right
  |> add_transition "make_end" ' ' "end_init" 'E' Left

  |> add_transition "end_init" '1' "end_init" '1' Left
  |> add_transition "end_init" 'A' "first_b" 'A' Right
  |> add_transition "first_b" '1' "write" 'B' Left

  |> add_transition "write" '1' "write" '1' Left
  |> add_transition "write" 'A' "write" 'A' Left
  |> add_transition "write" 'B' "write" 'B' Left
  |> add_transition "write" 'E' "write" 'E' Left
  |> add_transition "write" 'C' "write" 'C' Left
  |> add_transition "write" 'D' "write" 'D' Left
  |> add_transition "write" ' ' "end_write" '1' Right
  |> add_transition "end_write" '1' "end_write" '1' Right
  |> add_transition "end_write" 'A' "find_d" 'A' Right

  |> add_transition "find_d" '1' "find_d" '1' Right
  |> add_transition "find_d" 'C' "find_d" 'C' Right
  |> add_transition "find_d" 'D' "new_d" '1' Right
  |> add_transition "find_d" 'B' "new_d" 'C' Right

  |> add_transition "new_d" '1' "write" 'D' Left
  |> add_transition "new_d" 'C' "write" 'B' Left 
  |> add_transition "new_d" 'E' "find_c" 'E' Left

  |> add_transition "find_c" '1' "find_c" '1' Left
  |> add_transition "find_c" 'C' "new_c" '1' Right

  |> add_transition "new_c" '1' "reset_d" 'C' Left
  |> add_transition "new_c" 'E' "cleanup" ' ' Left
  
  |> add_transition "reset_d" '1' "reset_d" '1' Left
  |> add_transition "reset_d" 'A' "new_d" 'A' Right

  |> add_transition "cleanup" '1' "cleanup" ' ' Left
  |> add_transition "cleanup" 'A' "quit" ' ' Left
)

let kvadrat s = MachineUcinkovito.speed_run kvadrat_stroj s

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme število `n`,
  zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih
  natanko `n` enic.
[*----------------------------------------------------------------------------*)

let enice_stroj : MachineUcinkovito.t = MachineUcinkovito.(
  make "start" []
  |> add_transition "start" '1' "make_border" '1' Left
  |> add_transition "start" '0' "make_border" '0' Left
  |> add_transition "make_border" ' ' "next" 'A' Right

  |> add_transition "next" 'Z' "next" 'Z' Right
  |> add_transition "next" 'O' "next" 'O' Right
  |> add_transition "next" '0' "double" 'Z' Left
  |> add_transition "next" '1' "double_add" 'O' Left
  |> add_transition "next" ' ' "cleanup" ' ' Left


  |> add_transition "double" 'O' "double" 'O' Left
  |> add_transition "double" 'Z' "double" 'Z' Left
  |> add_transition "double" 'A' "double" 'A' Left
  |> add_transition "double" '1' "double" '1' Left
  |> add_transition "double" ' ' "double_mark" ' ' Right

  |> add_transition "double_mark" '1' "write" 'O' Left
  |> add_transition "double_mark" 'A' "next" 'A' Right

  |> add_transition "write" '1' "write" '1' Left
  |> add_transition "write" ' ' "back" '1' Right
  |> add_transition "back" '1' "back" '1' Right
  |> add_transition "back" 'O' "double_mark" '1' Right


  |> add_transition "double_add" 'O' "double_add" 'O' Left
  |> add_transition "double_add" 'Z' "double_add" 'Z' Left
  |> add_transition "double_add" 'A' "double_add" 'A' Left
  |> add_transition "double_add" '1' "double_add" '1' Left
  |> add_transition "double_add" ' ' "double_add_mark" ' ' Right

  |> add_transition "double_add_mark" '1' "write_add" 'O' Left
  |> add_transition "double_add_mark" 'A' "write_add" 'A' Left

  |> add_transition "write_add" '1' "write_add" '1' Left
  |> add_transition "write_add" ' ' "back_add" '1' Right
  |> add_transition "back_add" '1' "back_add" '1' Right
  |> add_transition "back_add" 'O' "double_add_mark" '1' Right
  |> add_transition "back_add" 'A' "next" 'A' Right

  |> add_transition "cleanup" 'Z' "cleanup" ' ' Left
  |> add_transition "cleanup" 'O' "cleanup" ' ' Left
  |> add_transition "cleanup" 'A' "quit" ' ' Left
)

let enice s = MachineUcinkovito.speed_run enice_stroj s

(*----------------------------------------------------------------------------*
  Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
  sprejme število `n` enic, na koncu pa naj bo na traku zapisano število `n`
  v dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let dvojiski_stroj : MachineUcinkovito.t = MachineUcinkovito.(
  make "start" []
  |> add_transition "start" '1' "start" '1' Left
  |> add_transition "start" ' ' "div" 'A' Right

  |> add_transition "div" '1' "erase" 'O' Right
  |> add_transition "div" ' ' "even_cleanup" ' ' Left

  |> add_transition "erase" '1' "erase" '1' Right
  |> add_transition "erase" ' ' "erase2" ' ' Left

  |> add_transition "erase2" '1' "back" ' ' Left
  |> add_transition "erase2" 'O' "odd_cleanup" ' ' Left

  |> add_transition "back" '1' "back" '1' Left
  |> add_transition "back" 'O' "div" 'O' Right

  |> add_transition "even_cleanup" 'O' "even_cleanup" '1' Left
  |> add_transition "even_cleanup" 'A' "even_cleanup" 'A' Left
  |> add_transition "even_cleanup" '0' "even_cleanup" '0' Left
  |> add_transition "even_cleanup" '1' "even_cleanup" '1' Left
  |> add_transition "even_cleanup" ' ' "return" '0' Right


  |> add_transition "odd_cleanup" 'O' "odd_cleanup" '1' Left
  |> add_transition "odd_cleanup" 'A' "odd_cleanup" 'A' Left
  |> add_transition "odd_cleanup" '0' "odd_cleanup" '0' Left
  |> add_transition "odd_cleanup" '1' "odd_cleanup" '1' Left
  |> add_transition "odd_cleanup" ' ' "return" '1' Right

  |> add_transition "return" '0' "return" '0' Right
  |> add_transition "return" '1' "return" '1' Right
  |> add_transition "return" 'A' "is_empty" 'A' Right

  |> add_transition "is_empty" ' ' "quit" ' ' Left
  |> add_transition "is_empty" '1' "return2" '1' Left

  |> add_transition "return2" 'A' "div" 'A' Right

  |> add_transition "quit" 'A' "quit_for_real" ' ' Left
)

let dvojisko s = MachineUcinkovito.speed_run dvojiski_stroj s

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme oklepaje `(` in
  `)`, `[` in `]` ter `{` in `}`. Stroj naj na traku izpiše `1`, če so
  oklepaji pravilno uravnoteženi in gnezdeni, ter `0` sicer.
[*----------------------------------------------------------------------------*)

let uravnotezeni_oklepaji_stroj : MachineUcinkovito.t = MachineUcinkovito.(
  make "start" []
  |> add_transition "start" '(' "okrogli1_b" '(' Right
  |> add_transition "start" ')' "okrogli2_b" ')' Right
  |> add_transition "start" '[' "oglati1_b" '[' Right
  |> add_transition "start" ']' "oglati2_b" ']' Right
  |> add_transition "start" '{' "zaviti1_b" '{' Right
  |> add_transition "start" '}' "zaviti2_b" '}' Right
  |> add_transition "start" 'A' "start" 'A' Right
  |> add_transition "start" ' ' "cleanup" ' ' Left

  |> add_transition "cleanup" 'A' "cleanup" ' ' Left
  |> add_transition "cleanup" ' ' "quit" '1' Right


  |> add_transition "okrogli1_b" '(' "okrogli1_b" '(' Right
  |> add_transition "okrogli1_b" ')' "okrogli2_patch" 'A' Left
  |> add_transition "okrogli1_b" '[' "oglati1_b" '[' Right
  |> add_transition "okrogli1_b" ']' "oglati2_b" ']' Right
  |> add_transition "okrogli1_b" '{' "zaviti1_b" '{' Right
  |> add_transition "okrogli1_b" '}' "zaviti2_b" '}' Right
  |> add_transition "okrogli1_b" 'A' "okrogli1_b" 'A' Right
  |> add_transition "okrogli1_b" ' ' "back_b" ' ' Left

  |> add_transition "okrogli2_patch" 'A' "okrogli2_patch" 'A' Left
  |> add_transition "okrogli2_patch" '(' "okrogli2_g" 'A' Right

  |> add_transition "okrogli1_g" '(' "okrogli1_g" '(' Right
  |> add_transition "okrogli1_g" ')' "okrogli2_g" ')' Right
  |> add_transition "okrogli1_g" '[' "oglati1_g" '[' Right
  |> add_transition "okrogli1_g" ']' "oglati2_g" ']' Right
  |> add_transition "okrogli1_g" '{' "zaviti1_g" '{' Right
  |> add_transition "okrogli1_g" '}' "zaviti2_g" '}' Right
  |> add_transition "okrogli1_g" 'A' "okrogli1_g" 'A' Right
  |> add_transition "okrogli1_g" ' ' "back_g" ' ' Left

  |> add_transition "okrogli2_b" '(' "okrogli1_b" '(' Right
  |> add_transition "okrogli2_b" ')' "okrogli2_b" ')' Right
  |> add_transition "okrogli2_b" '[' "oglati1_b" '[' Right
  |> add_transition "okrogli2_b" ']' "oglati2_b" ']' Right
  |> add_transition "okrogli2_b" '{' "zaviti1_b" '{' Right
  |> add_transition "okrogli2_b" '}' "zaviti2_b" '}' Right
  |> add_transition "okrogli2_b" 'A' "okrogli2_b" 'A' Right
  |> add_transition "okrogli2_b" ' ' "back_b" ' ' Left

  |> add_transition "okrogli2_g" '(' "okrogli1_g" '(' Right
  |> add_transition "okrogli2_g" ')' "okrogli2_g" ')' Right
  |> add_transition "okrogli2_g" '[' "oglati1_g" '[' Right
  |> add_transition "okrogli2_g" ']' "oglati2_g" ']' Right
  |> add_transition "okrogli2_g" '{' "zaviti1_g" '{' Right
  |> add_transition "okrogli2_g" '}' "zaviti2_g" '}' Right
  |> add_transition "okrogli2_g" 'A' "okrogli2_g" 'A' Right
  |> add_transition "okrogli2_g" ' ' "back_g" ' ' Left


  |> add_transition "oglati1_b" '(' "okrogli1_b" '(' Right
  |> add_transition "oglati1_b" ')' "okrogli2_b" ')' Right
  |> add_transition "oglati1_b" '[' "oglati1_b" '[' Right
  |> add_transition "oglati1_b" ']' "oglati2_patch" 'A' Left
  |> add_transition "oglati1_b" '{' "zaviti1_b" '{' Right
  |> add_transition "oglati1_b" '}' "zaviti2_b" '}' Right
  |> add_transition "oglati1_b" 'A' "oglati1_b" 'A' Right
  |> add_transition "oglati1_b" ' ' "back_b" ' ' Left

  |> add_transition "oglati2_patch" 'A' "oglati2_patch" 'A' Left
  |> add_transition "oglati2_patch" '[' "oglati2_g" 'A' Right

  |> add_transition "oglati1_g" '(' "okrogli1_g" '(' Right
  |> add_transition "oglati1_g" ')' "okrogli2_g" ')' Right
  |> add_transition "oglati1_g" '[' "oglati1_g" '[' Right
  |> add_transition "oglati1_g" ']' "oglati2_g" ']' Right
  |> add_transition "oglati1_g" '{' "zaviti1_g" '{' Right
  |> add_transition "oglati1_g" '}' "zaviti2_g" '}' Right
  |> add_transition "oglati1_g" 'A' "oglati1_g" 'A' Right
  |> add_transition "oglati1_g" ' ' "back_g" ' ' Left

  |> add_transition "oglati2_b" '(' "okrogli1_b" '(' Right
  |> add_transition "oglati2_b" ')' "okrogli2_b" ')' Right
  |> add_transition "oglati2_b" '[' "oglati1_b" '[' Right
  |> add_transition "oglati2_b" ']' "oglati2_b" ']' Right
  |> add_transition "oglati2_b" '{' "zaviti1_b" '{' Right
  |> add_transition "oglati2_b" '}' "zaviti2_b" '}' Right
  |> add_transition "oglati2_b" 'A' "oglati2_b" 'A' Right
  |> add_transition "oglati2_b" ' ' "back_b" ' ' Left

  |> add_transition "oglati2_g" '(' "okrogli1_g" '(' Right
  |> add_transition "oglati2_g" ')' "okrogli2_g" ')' Right
  |> add_transition "oglati2_g" '[' "oglati1_g" '[' Right
  |> add_transition "oglati2_g" ']' "oglati2_g" ']' Right
  |> add_transition "oglati2_g" '{' "zaviti1_g" '{' Right
  |> add_transition "oglati2_g" '}' "zaviti2_g" '}' Right
  |> add_transition "oglati2_g" 'A' "oglati2_g" 'A' Right
  |> add_transition "oglati2_g" ' ' "back_g" ' ' Left


  |> add_transition "zaviti1_b" '(' "okrogli1_b" '(' Right
  |> add_transition "zaviti1_b" ')' "okrogli2_b" ')' Right
  |> add_transition "zaviti1_b" '[' "oglati1_b" '[' Right
  |> add_transition "zaviti1_b" ']' "oglati2_b" ']' Right
  |> add_transition "zaviti1_b" '{' "zaviti1_b" '{' Right
  |> add_transition "zaviti1_b" '}' "zaviti2_patch" 'A' Left
  |> add_transition "zaviti1_b" 'A' "zaviti1_b" 'A' Right
  |> add_transition "zaviti1_b" ' ' "back_b" ' ' Left

  |> add_transition "zaviti2_patch" 'A' "zaviti2_patch" 'A' Left
  |> add_transition "zaviti2_patch" '{' "zaviti2_g" 'A' Right

  |> add_transition "zaviti1_g" '(' "okrogli1_g" '(' Right
  |> add_transition "zaviti1_g" ')' "okrogli2_g" ')' Right
  |> add_transition "zaviti1_g" '[' "oglati1_g" '[' Right
  |> add_transition "zaviti1_g" ']' "oglati2_g" ']' Right
  |> add_transition "zaviti1_g" '{' "zaviti1_g" '{' Right
  |> add_transition "zaviti1_g" '}' "zaviti2_g" '}' Right
  |> add_transition "zaviti1_g" 'A' "zaviti1_g" 'A' Right
  |> add_transition "zaviti1_g" ' ' "back_g" ' ' Left

  |> add_transition "zaviti2_b" '(' "okrogli1_b" '(' Right
  |> add_transition "zaviti2_b" ')' "okrogli2_b" ')' Right
  |> add_transition "zaviti2_b" '[' "oglati1_b" '[' Right
  |> add_transition "zaviti2_b" ']' "oglati2_b" ']' Right
  |> add_transition "zaviti2_b" '{' "zaviti1_b" '{' Right
  |> add_transition "zaviti2_b" '}' "zaviti2_b" '}' Right
  |> add_transition "zaviti2_b" 'A' "zaviti2_b" 'A' Right
  |> add_transition "zaviti2_b" ' ' "back_b" ' ' Left

  |> add_transition "zaviti2_g" '(' "okrogli1_g" '(' Right
  |> add_transition "zaviti2_g" ')' "okrogli2_g" ')' Right
  |> add_transition "zaviti2_g" '[' "oglati1_g" '[' Right
  |> add_transition "zaviti2_g" ']' "oglati2_g" ']' Right
  |> add_transition "zaviti2_g" '{' "zaviti1_g" '{' Right
  |> add_transition "zaviti2_g" '}' "zaviti2_g" '}' Right
  |> add_transition "zaviti2_g" 'A' "zaviti2_g" 'A' Right
  |> add_transition "zaviti2_g" ' ' "back_g" ' ' Left


  |> add_transition "back_b" '(' "back_b" ' ' Left
  |> add_transition "back_b" ')' "back_b" ' ' Left
  |> add_transition "back_b" '[' "back_b" ' ' Left
  |> add_transition "back_b" ']' "back_b" ' ' Left
  |> add_transition "back_b" '{' "back_b" ' ' Left
  |> add_transition "back_b" '}' "back_b" ' ' Left
  |> add_transition "back_b" 'A' "back_b" ' ' Left
  |> add_transition "back_b" ' ' "quit" '0' Right

  |> add_transition "back_g" '(' "back_g" '(' Left
  |> add_transition "back_g" ')' "back_g" ')' Left
  |> add_transition "back_g" '[' "back_g" '[' Left
  |> add_transition "back_g" ']' "back_g" ']' Left
  |> add_transition "back_g" '{' "back_g" '{' Left
  |> add_transition "back_g" '}' "back_g" '}' Left
  |> add_transition "back_g" 'A' "back_g" 'A' Left
  |> add_transition "back_g" ' ' "start" ' ' Right
)

let oklepaji s = MachineUcinkovito.speed_run uravnotezeni_oklepaji_stroj s
