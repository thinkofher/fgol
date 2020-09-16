open System
open System.Threading

type Point = Point of int * int

let pointToTuple ((Point (x, y))) = (x, y)

let addPoints p1 p2 =
    match (p1, p2) with
    | ((Point (a, b), Point (c, d))) -> Point(a + c, b + d)

type State =
    | Dead
    | Alive

let showState: State -> string =
    function
    | Dead -> " "
    | Alive -> "●"

let randomState (rand: Random): State =
    match rand.Next(0, 2) with
    | 0 -> Dead
    | _ -> Alive

let neighboursRelativePositions =
    [ Point(-1, 0)
      Point(-1, 1)
      Point(0, 1)
      Point(1, 1)
      Point(1, 0)
      Point(1, -1)
      Point(0, -1)
      Point(-1, -1) ]

let calcNeighboursPositions pos =
    neighboursRelativePositions
    |> List.map (addPoints pos)

let swap f a b = f b a

let neighbours (m: Map<Point, State>) (p: Point): State list =
    p
    |> calcNeighboursPositions
    |> List.map (swap Map.tryFind m)
    |> List.filter Option.isSome
    |> List.map Option.get

let howManyNeighbours (m: Map<Point, State>) (p: Point): int =
    neighbours m p
    |> List.filter (fun s -> s = Alive)
    |> List.length

let nextState (s: State) (neighboursNum: int): State =
    match s with
    | Alive when neighboursNum = 2 || neighboursNum = 3 -> Alive
    | Dead when neighboursNum = 3 -> Alive
    | _ -> Dead

let nextTableState (m: Map<Point, State>): Map<Point, State> =
    Map.map (fun k v -> nextState v (howManyNeighbours m k)) m

let randomizeTableState (rand: Random) = Map.map (fun _ _ -> (randomState rand))

let tableSize (m: Map<Point, State>): (int * int) =
    m
    |> Map.toList
    |> List.map fst
    |> List.max
    |> pointToTuple

let genTable (height: int) (width: int) : (int * int) list list =
    [ 0 .. height ]
    |> List.rev
    |> List.map (fun n -> List.replicate (width + 1) n)
    |> List.map (fun l -> List.zip [ 0 .. width ] l)

let flatten<'a> : 'a list list -> 'a list =
    List.fold (fun acc l -> List.concat [ acc; l ]) []

let newTableState (height: int) (width: int): Map<Point, State> =
    genTable height width
    |> flatten
    |> List.map (fun (x, y) -> (Point(x, y), Dead))
    |> Map.ofList

let join (sep: string) (seq: seq<string>) = String.Join(sep, seq)

let showTable (m: Map<Point, State>): string =
    let width, height = tableSize m

    genTable height width
    |> List.map (List.map (fun (x, y) -> Map.tryFind (Point(x, y)) m))
    |> List.map (List.map (fun s -> Option.get s))
    |> List.map (List.map showState)
    |> List.map (join "")
    |> join "\n"

let previewTable (m: Map<Point, State>): Map<Point, State> =
    showTable m |> printfn "%s"
    m

let isTableEmpty<'K when 'K: comparison> : Map<'K, State> -> bool = Map.forall (fun _ v -> v = Dead)

let transformTableIfEmpty (f: (Map<Point, State> -> Map<Point, State>)) (m: Map<Point, State>): Map<Point, State> =
    match m |> isTableEmpty with
    | true -> f m
    | false -> m

let returnCode code _ = code

[<EntryPoint>]
let main argv =
    Console.Clear()
    let rand = Random()

    let mutable m =
        newTableState 15 30 |> randomizeTableState rand

    while true do
        m <-
            m
            |> previewTable
            |> nextTableState
            |> transformTableIfEmpty (randomizeTableState rand)

        Thread.Sleep(100)
        Console.Clear()

    0
