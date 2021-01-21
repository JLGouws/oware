module Oware

type StartingPosition =
  | South
  | North

type House =
  | House of int * House
  | Empty

type Board =
  | Board of (House) * StartingPosition * (int * int)

let getSeeds n board =
  let (Board (houses, _, _)) = board
  let rec fb c h =
    match n = c with
      | true -> 
        let (House(v, _)) = h
        v 
      | false -> 
        let (House(_, n)) = h
        (c + 1, n) ||> fb
  fb 1 houses

let useHouse n board =
  let bb b n =
    let rec eb b n c p =
      match c < 13 with
        | false -> 
          let (Board(_, q, s)) = b
          Board(Empty, q, s), p
        | true -> 
          let (Board(House(v, nh), q, s)) = b
          match n > 0 with
            | true ->
              match c = n with
                | true -> 
                  let (Board(h, q, s), p) = eb (Board(nh, q, s)) 0 (c + 1) v
                  Board(House(0, h), q, s), p
                | false -> 
                  let ((Board(h, q, s)), p) = eb (Board(nh, q, s)) n (c + 1) 0
                  Board(House(v, h), q, s), p
            | false ->
                  match p > 0 with
                    | true -> 
                      let ((Board(h, q, s))), p = eb (Board(nh, q, s)) n (c + 1) (p-1)
                      Board(House(v + 1, h), q, s), p
                    | false -> 
                      let ((Board(h, q, s))), p = eb (Board(nh, q, s)) n (c + 1) p 
                      Board(House(v, h), q, s), p

    let rec fb = function
        | (b, 0) -> b//, 0
        | (b, p) -> eb b 0 1 p |> fb
    //let r, 0 = 
    eb b n 1 0 |> fb
    //r

  let (Board(h, p, s)) = board
  match p with
    | South ->
      match 1 <= n && n <= 6 with
        | true -> bb board n
        | false -> board
    | North ->
      match 7 <= n && n <= 12 with
        | true -> bb board n
        | false -> board

let start position = 
  let rec mb c =
    match c < 12 with
      | true -> House(4, c + 1 |> mb)
      | false -> Empty
  Board(mb 0, position, (0, 0)) 

let score board =
  let (Board(_, _, s)) = board
  s

let gameState board =
  let (Board(_, p, _)) = board
  match p with
    | North -> "North's turn"
    | South -> "South's turn"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
