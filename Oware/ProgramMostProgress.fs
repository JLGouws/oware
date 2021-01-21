module Oware

type StartingPosition =
  | South
  | North

type House =
  | House of int * House
  | Empty

type Board =
  { 
    houses : House 
    pos : StartingPosition
    score : (int * int)
  }

let getSeeds n board =
  let {houses = h} = board
  let rec fb c h =
    match n = c with
      | true -> 
        let (House(v, _)) = h
        v 
      | false -> 
        let (House(_, n)) = h
        (c + 1, n) ||> fb
  fb 1 h

let useHouse n board =
  let bb b n =
    let rec eb b n c p t =
      match c < 13 with
        | false -> {b with houses = Empty}, p, t
        | true -> 
          let {houses = House(v, nh); pos = pla} = b
          match n > 0 with
            | true ->
              match c = n with
                | true -> 
                  match v = 0 with
                    | true ->  b, p, t
                    | false ->
                      match pla with
                        | South -> 
                          let ({houses = h} as b), p, t = eb {b with houses = nh; pos = North} -n (c + 1) v t
                          {b with houses = House(0, h)}, p, t
                        | North -> 
                          let ({houses = h} as b), p, t = eb {b with houses = nh; pos = South} -n (c + 1) v t
                          {b with houses = House(0, h)}, p, t
                | false -> 
                  let ({houses = h} as b), p, t = eb {b with houses = nh} n (c + 1) 0 t
                  {b with houses = House(v, h)}, p, t
            | false ->
                  match p > 0 with
                    | false -> 
                      let ({houses = h} as b), p, t = eb {b with houses = nh} n (c + 1) p t
                      {b with houses = House(v, h)}, p, t
                    | true ->
                      match -n = c with
                        | true ->
                          let ({houses = h; pos = q; score = s} as b), p, t = eb {b with houses = nh} n (c + 1) p t
                          {b with houses = House(v, h)}, p, t
                        | false ->
                          match p = 1 with
                            | true -> 
                              let ({houses = h; pos = q; score = s} as b), p, t = eb {b with houses = nh} n (c + 1) (p-1) t
                              match v + 1 with
                                | 3 | 2 ->
                                  let s1, s2 = s
                                  match q with
                                    | South -> {b with houses = House(0, h); score = (s1, s2 + v + 1)}, p, true
                                    | North -> {b with houses = House(0, h); score = (s1 + v + 1, s2)}, p, true
                                | x -> {b with houses = House(x, h)}, p, false
                            | false -> 
                              let ({houses = h; pos = q; score = s} as b), p, t = eb {b with houses = nh} n (c + 1) (p-1) t
                              match t with
                                | true ->
                                  match v + 1 with
                                    | 3 | 2 ->
                                      let s1, s2 = s
                                      match q with
                                        | South -> {b with houses = House(0, h); score = (s1, s2 + v + 1)}, p, true
                                        | North -> {b with houses = House(0, h); score = (s1 + v + 1, s2)}, p, true
                                    | x -> {b with houses = House(x, h)}, p, false
                                | false -> {b with houses = House(v + 1, h)}, p, t

    let rec fb = function
        | (b, 0, _) -> b
        | (b, p, t) -> eb b -n 1 p t |> fb
    eb b n 1 0 false |> fb

  let {pos = p} = board
  match p with
    | South ->
      match 1 <= n && n <= 6 with
        | true -> bb board n
        | false -> board
    | North ->
      match 7 <= n && n <= 12 with
        | true -> 
          bb board n
        | false -> board

let start position = 
  let rec mb c =
    match c < 12 with
      | true -> House(4, c + 1 |> mb)
      | false -> Empty
  {houses = mb 0; pos = position; score = (0, 0)} 

let score board =
  let {score = s} = board
  s

let gameState board =
  let {pos = p; score = (s1, s2)} = board
  match s1, s2 with
    | 24, 24 -> "Game ended in a draw"
    | x, y ->
      match p with
        | North -> 
          match s2 >= 25 with
            | true -> "South won"
            | false -> "North's turn"
        | South -> 
          match s1 >= 25 with
            | true -> "North won"
            | false -> "South's turn"

let printBoard b =
  printfn "(%d, %d, %d, %d, %d, %d,\n %d, %d, %d, %d, %d, %d)\n" (getSeeds 12 b) (getSeeds 11 b) (getSeeds 10 b) (getSeeds 9 b) (getSeeds 8 b) (getSeeds 7 b) (getSeeds 1 b) (getSeeds 2 b) (getSeeds 3 b) (getSeeds 4 b) (getSeeds 5 b) (getSeeds 6 b) 

let playGame numbers =
     let rec play xs game =
         match xs with
         | [] -> game
         | x::xs -> 
            gameState game |> printfn "%s"
            printBoard game
            printfn "%d" x
            score game ||> printfn "%d %d"
            play xs (useHouse x game)
     play numbers (start South)

[<EntryPoint>]
let main _ =
    let b = playGame [1; 7; 2; 9; 3; 10; 1; 11; 2; 9; 4; 7; 5; 12; 3; 11; 6]
    //) ||> printfn "%d %d"
    printBoard b
    gameState b |> printfn "%s"
    0 // return an integer exit code
