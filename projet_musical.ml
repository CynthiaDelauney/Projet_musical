
type objet_musical =
    | Note of int * int * int
    | Silence of int
    | Parallele of objet_musical list
    | Sequence of objet_musical list 

let s1 = Sequence((Note(60,1000,65))::(Note (64,500,65))::(Note (62,500,65))::(Silence(1000))
            ::(Note (67,1000,65))::[])
let s2 = Sequence((Note(53,2000,65))::(Note (57,1000,65))::(Note (57,1000,65))::[])
let exemple = Parallele([s1;s2])

let max e1 e2 = if e1 > e2 then e1 else e2 

(* 
 * duration rend la durée de l'objet musical en ms
 *)

let rec duration obj =
    match obj with
        | Note(_, d, _) -> d
        | Silence d     -> d
        | Sequence l    -> List.fold_left (+) 0 (List.map duration l)
        | Parallele l   -> List.fold_left max  0 (List.map duration l) 

let rec copy obj =
    match obj with
        | Note(a, b, c) -> Note(a, b, c)
        | Silence d     -> Silence(d)
        | Sequence l    -> Sequence(List.map copy l)
        | Parallele l   -> Parallele(List.map copy l) 

(* 
 * note_count rend le nombre de note dans l'objet musical
 *)

let rec note_count obj =
    match obj with
        | Note _ | Silence _ -> 1
        | Sequence l         -> List.fold_left (+) 0 (List.map note_count l)
        | Parallele l        -> List.fold_left (+) 0 (List.map note_count l) 

(* 
 * (stretch f obj) multiplie la durée de toutes les notes dans obj de 'f' ms
 *)

let rec stretch = function f -> function obj ->
    match obj with
        | Note(a, b, c) -> Note(a, (int_of_float ((float_of_int b) *. f)), c)
        | Silence d     -> Silence((int_of_float ((float_of_int d) *. f)))
        | Sequence l    -> Sequence(List.map (stretch f) l)
        | Parallele l   -> Parallele(List.map (stretch f) l) 

(*
 * transpose rend la transposée de l'objet musical
 *)

let rec transpose n obj =
    match obj with
        | Note(a, b, c) -> Note(a + n, b, c)
        | Silence d     -> Silence(d)
        | Sequence l    -> Sequence(List.map (transpose n) l)
        | Parallele l   -> Parallele(List.map (transpose n) l) 

let rec retrograde obj =
    match obj with
        | Note(a, b, c)    -> Note(a, b, c)
        | Silence d        -> Silence(d)
        | Sequence l       -> Sequence(List.rev(List.map (retrograde) l))
        | Parallele l      -> Parallele(List.rev (List.map (retrograde) l)) 

let rec mirror obj =
    match obj with
        | Note(a, b, c)   -> Note(a - 2 * (a - 60), b, c)
        | Silence d       -> Silence(d)
        | Sequence l      -> Sequence(List.map (mirror) l)
        | Parallele l     -> Parallele(List.map (mirror) l) 

(*
 * repeat rend un objet musical dans lequel 'obj' serra répété 'nfr' fois
 *)

let rec repeat obj nbr =
    if (nbr = 0 || nbr = 1) 
    then obj
    else Sequence(obj::(repeat obj (nbr - 1))::[]) 

let rec canon = function decalage -> function obj -> 
    Parallele(obj::Sequence(Silence(decalage)::obj::[])::[]) 

let concat_mo = function mo1 -> function mo2 ->
    Sequence(mo1::mo2::[]) 

(* ============================================================================ *)

(* Partie graphique : Piano-roll *)

let init_graph = 
    Graphics.open_graph " 1000x300" ;;
    (* 2 pixels pour l'hauteur , pour la longeur : 1 pixel pour 25 ms *)

let random_color () = 
    match (Random.int 7) with
    | 0 -> Graphics.black
    | 1 -> Graphics.red
    | 2 -> Graphics.green
    | 3 -> Graphics.blue
    | 4 -> Graphics.yellow
    | 5 -> Graphics.cyan
    | _ -> Graphics.magenta 

let draw_note hauteur duree debut =
    Graphics.set_color (random_color ()) ;
    Graphics.fill_rect (debut / 25 ) (hauteur * 2) (duree / 25) 2 ;
    debut + duree

let rec draw_om_list = function debut -> function
    | []   -> debut 
    | a::q -> (draw_om_list (draw_om debut a) q)
and
    draw_om = function debut -> function
    | Note(h, d, v)                     -> draw_note h d debut
    | Silence d                         -> debut + d
    | Sequence []                       -> debut
    | Parallele []                      -> debut
    | Sequence l                        -> (draw_om_list debut l) 
    | Parallele l                       -> (List.fold_left (max) 0 (List.map (draw_om debut) l)) 

let continue () =
    let ev = Graphics.wait_next_event [Graphics.Key_pressed] in 
        (if ev.Graphics.key = 'q' then false else true) 

(* ============================================================================ *)

(* Partie MIDI *)

let rec gen_liste_note_aux debut = function
    | a::[] -> (gen_liste_note debut a)
    | a::q  -> (gen_liste_note debut a)@(gen_liste_note_aux (debut + (duration a)) q) 
and  gen_liste_note debut = function
    | Note(a, b , c)   -> (debut, 1, Midi.NoteON(a, c))::[(debut + b), 1, Midi.NoteOFF(a, c)] 
    | Silence d        -> (debut, 1, Midi.NoteON(0, 0))::[(debut + d), 1, Midi.NoteOFF(0, 0)]    
    | Sequence(a::[])  -> (gen_liste_note debut a)
    | Sequence(a::l)   -> (gen_liste_note debut a)@(gen_liste_note_aux (debut + (duration a)) l)
    | Parallele(a::[]) -> (gen_liste_note debut a)
    | Parallele(a::l)  -> (gen_liste_note debut a)@(gen_liste_note_aux debut l) ;;

let rec gen_midi_aux debut = function
    | []            -> []
    | (a, b, c)::q  -> ((a - debut), b, c)::(gen_midi_aux a q) ;;

let sort_list l =
    (List.sort (fun a x -> if a > x then 1 else (if a = x then 0 else -1)) l) ;;

let gen_midi l = (gen_midi_aux 0 (sort_list l)) ;;

(* fonction qui contruit le canon de bach *)
let bach voix1 voix2 =
    let voix3 = (transpose 7 voix2) in
    (Parallele ( 
            Sequence(voix1::(transpose 2 voix1)::(transpose 4 voix1)::(transpose 6 voix1)::
                (transpose 8 voix1)::[(transpose 10 voix1)]) ::  
            Sequence(voix2::(transpose 2 voix2)::(transpose 4 voix2)::(transpose 6 voix2)::
                (transpose 8 voix2)::[(transpose 10 voix2)]) :: 
            [
            Sequence((Silence 4000)::voix3::(transpose 2 voix3)::(transpose 4 voix3)::
                (transpose 6 voix3)::(transpose 8 voix3)::[(transpose 10 voix3)]) 
            ] )) ;;

let voix1 = Sequence[
        Note(48, 1500, 100);  Note(50, 500, 100);   Note(51, 500, 100);
        Note(52, 500, 100);   Note(53, 500, 100);   Note(54, 500, 1000);
        Note(55, 2000, 100);  Note(56, 1000, 100);  Note(56, 250, 100);
        Note(53, 250, 100);   Note(49, 250, 100);   Note(48, 250, 100);
        Note(47, 1000, 100);  Silence 1000;         Silence 1000;
        Note(55, 1000, 100);  Note(55, 1000, 100);  Note(54, 2000, 100);
        Note(54, 1000, 100);  Note(54, 1000, 100);  Note(52, 2000, 100);
        Note(52, 1000, 100);  Note(52, 1000, 100);  Note(50, 1000, 100);
        Note(50, 500, 100);   Note(49, 500, 100);   Note(46, 500, 100);
        Note(45, 500, 100);   Note(50, 1000, 100);  Silence 1000;
        Silence 1000;         Note(55, 1000, 100);  Note(55, 1000, 100);
        Note(53, 1000, 100);  Note(52, 2000, 100)] ;;

let voix2 = Sequence[
        Silence 250;         Note(36, 250, 100);   Note(39, 250, 100);
        Note(43, 250, 100);  Note(48, 2000, 100);  Note(46, 500, 100);
        Note(45, 500, 100);  Note(46, 1000, 100) ; Note(46, 250, 100);
        Note(40, 250, 100);  Note(38, 250, 100) ;  Note(40, 250, 100);
        Note(41, 250, 100);  Note(36, 250, 100) ;  Note(41, 250, 100);
        Note(43, 250, 100);  Note(44, 1000, 100) ; Note(44, 500, 100);
        Note(44, 500, 100);  Note(43, 500, 100) ;  Note(41, 500, 100);
        Note(39, 1000, 100); Note(39, 250, 100) ;  Note(39, 250, 100);
        Note(41, 250, 100);  Note(39, 250, 100);   Note(38, 500, 100);
        Note(36, 500, 100);  Note(37, 1000, 100);  Silence 500;
        Note(38, 500, 100);  Note(39, 500, 100);   Note(38, 500, 100);
        Note(36, 250, 100);  Note(35, 250, 100);   Note(36, 250, 100);
        Note(35, 250, 100);  Note(36, 250, 100);   Note(38, 250, 100);
        Note(36, 250, 100);  Note(35, 250, 100);   Note(33, 250, 100);
        Note(31, 250, 100);  Note(33, 250, 100);   Note(35, 250, 100);
        Note(36, 250, 100);  Note(33, 250, 100);   Note(35, 250, 100);
        Note(36, 250, 100);  Note(38, 500, 100);   Note(48, 500, 100);
        Note(46, 250, 100);  Note(45, 250, 100);   Note(46, 500, 100);
        Note(43, 500, 100);  Note(40, 500, 100);   Note(45, 250, 100);
        Note(43, 250, 100);  Note(42, 500, 100);   Note(43, 250, 100);
        Note(45, 250, 100);  Note(46, 500, 100);   Note(37, 500, 100);
        Note(38, 500, 100);  Silence 1000;         Silence 500;
        Note(38, 500, 100);  Note(38, 250, 100);   Note(41, 250, 100);
        Note(40, 250, 100);  Note(38, 250, 100);   Note(37, 250, 100);
        Note(38, 250, 100);  Note(40, 250, 100);   Note(41, 250, 100);
        Note(43, 250, 100);  Note(46, 250, 100);   Note(45, 250, 100);
        Note(43, 250, 100)] ;;

let () =
    Random.init (int_of_float (Sys.time ())) ;
    init_graph ;

    let om_bach = (bach voix1 voix2) in
    let bach = gen_midi (gen_liste_note 0 om_bach) in 

    draw_om 0 om_bach ;

    while continue () do 
        ()
    done ;
    Graphics.close_graph () ;

    Midi.write (1000 , [bach]) "test_Cynthia_Delauney.mid" ;;
