open System.IO
open System

type Block =
    | OPEN
    | WALL

type Dir =
    | EAST
    | WEST
    | SOUTH
    | NORTH

let turnLeft (dir: Dir) =
    match dir with
    | EAST -> NORTH
    | NORTH -> WEST
    | WEST -> SOUTH
    | SOUTH -> EAST

let turnRight (dir: Dir) =
    match dir with
    | EAST -> SOUTH
    | SOUTH -> WEST
    | WEST -> NORTH
    | NORTH -> EAST

type Inst =
    | Left
    | Right
    | Steps of int

type CubeInfo(equatorMinY, equatorMaxY, prePoleMinX, prePoleMaxX, poleMinX, poleMaxX, maxY:int, maxX:int) =
    member this.EquatorMaxY = equatorMaxY
    member this.EquatorMinY = equatorMinY
    member this.PoleMinX = poleMinX
    member this.PoleMaxX = poleMaxX
    member this.PrePoleMinX = prePoleMinX
    member this.PrePoleMaxX = prePoleMaxX
    member this.maxX = maxX
    member this.maxY = maxY

    override this.ToString() =
        $"CubeInfo(eq:{equatorMinY}-{equatorMaxY} prePole:{prePoleMinX}-{prePoleMaxX} pole:{poleMinX}-{poleMaxX})"

type Pos = int * int

type Instructions(s: Inst list) =
    member this.Inst = s
    override this.ToString() = $"Instructions({s})"

let nextPos (dir: Dir) ((x, y): Pos) : Pos =
    match dir with
    | NORTH -> x, y - 1
    | SOUTH -> x, y + 1
    | WEST -> x - 1, y
    | EAST -> x + 1, y

type Area(area: Map<Pos, Block>, south: int, east: int) =
    member this.Area = area
    member this.StartTile: Pos = area.Keys |> Seq.filter (fun (_, y) -> y = 0) |> Seq.min

    member this.Available(pos: Pos) = area.TryFind pos = Some(OPEN)

    member this.NextTile ((x, y): Pos) (dir: Dir) : Pos =
        let (x, y) = nextPos dir (x, y)

        if area.ContainsKey((x, y)) then
            (x, y)
        else
            match dir with
            | EAST ->
                let next = (if x > east then -1 else x), y
                this.NextTile next dir
            | WEST ->
                let next = (if x < 0 then east else x), y
                this.NextTile next dir
            | SOUTH ->
                let next = x, (if y > south then -1 else y)
                this.NextTile next dir
            | NORTH ->
                let next = x, (if y < 0 then south else y)
                this.NextTile next dir

    member this.NextCubeTile (cubeInfo: CubeInfo) ((x, y): Pos) (dir: Dir) : Dir * Pos =
        // printfn "NextCubeTile"
        let (x, y) = nextPos dir (x, y)

        if area.ContainsKey((x, y)) then
            dir, (x, y)
        else
            match dir with
            | EAST when cubeInfo.EquatorMinY > y -> // OK
                printfn $"Equator: North {(x, y)} {cubeInfo}"
                let y = cubeInfo.maxY - y
                let dir = dir |> turnLeft |> turnLeft
                this.NextCubeTile cubeInfo (x,y) dir
            | EAST when cubeInfo.EquatorMaxY < y -> // OK
                printfn $"Equator: South {(x, y)} {cubeInfo}"
                let x = y - cubeInfo.EquatorMaxY + cubeInfo.PoleMaxX
                let y = cubeInfo.EquatorMaxY
                let dir = NORTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | EAST -> // OK
                printfn $"Equator: At {(x, y)} cubeInfo={cubeInfo}"
                let x = x + cubeInfo.EquatorMaxY - y
                let y = cubeInfo.EquatorMaxY + 1
                let dir = SOUTH
                this.NextCubeTile cubeInfo (x, y) dir
            | WEST when y < cubeInfo.EquatorMinY -> // OK 
                printfn $"Equator: South {(x, y)} {cubeInfo}"
                let x = cubeInfo.PrePoleMinX + y
                let y = cubeInfo.EquatorMinY
                let dir = SOUTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | WEST when y > cubeInfo.EquatorMaxY -> // OK
                printfn $"Equator: South {(x, y)} {cubeInfo}"
                let x = cubeInfo.PrePoleMinX + y
                let y = cubeInfo.EquatorMaxY
                let dir = NORTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | WEST -> // OK (just wrapping)
                let next = (if x < 0 then east else x), y
                this.NextCubeTile cubeInfo next dir
            | SOUTH when x < cubeInfo.PrePoleMinX -> // OK // prePrePole, prePole, pole, PostPole
                let x = cubeInfo.PoleMaxX - x
                let y = cubeInfo.maxY
                let dir = NORTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | SOUTH when x < cubeInfo.PoleMinX -> // OK
                let y = cubeInfo.PrePoleMaxX - x + cubeInfo.EquatorMaxY + 1
                let x = cubeInfo.PoleMinX
                let dir = EAST 
                this.NextCubeTile cubeInfo (x,y) dir
            | SOUTH when x > cubeInfo.PoleMaxX ->
                let y = cubeInfo.EquatorMaxY + x - cubeInfo.PoleMaxX
                let x = cubeInfo.PoleMaxX
                let dir = WEST 
                this.NextCubeTile cubeInfo (x,y) dir
            | SOUTH -> // pole
                let x = cubeInfo.PrePoleMaxX - x
                let y = cubeInfo.EquatorMaxY
                let dir = NORTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | NORTH when x < cubeInfo.PrePoleMinX -> // prePrePole, prePole, pole, PostPole
                let x = cubeInfo.PoleMaxX - x
                let y = 0
                let dir = SOUTH 
                this.NextCubeTile cubeInfo (x,y) dir
            | NORTH when x < cubeInfo.PoleMinX ->
                let y = cubeInfo.EquatorMinY - cubeInfo.PoleMinX + x
                let x = cubeInfo.PoleMinX
                let dir = EAST
                this.NextCubeTile cubeInfo (x,y) dir
            | NORTH when x > cubeInfo.PoleMaxX ->
                let x = cubeInfo.EquatorMinY - x + cubeInfo.PoleMaxX
                let dir = WEST 
                this.NextCubeTile cubeInfo (x,y) dir
            | NORTH -> // at pole, just wrapping
                let next = x, (if y < 0 then south else y)
                this.NextCubeTile cubeInfo next dir



    override this.ToString() = $"Area({area})"


let initCubeInfo (area: Area) =
    let wrapX =
        area.Area.Keys |> Seq.filter (fun (x, y) -> y = 0) |> Seq.map fst |> Seq.max

    let maxY = area.Area.Keys |> Seq.map snd |> Seq.max
    let maxX = area.Area.Keys |> Seq.map fst |> Seq.max 

    let equatorMinY =
        area.Area.Keys |> Seq.filter (fun (x, y) -> x = 0) |> Seq.map snd |> Seq.min

    let equatorMaxY =
        area.Area.Keys |> Seq.filter (fun (x, y) -> x = 0) |> Seq.map snd |> Seq.max

    let poleMinX =
        area.Area.Keys |> Seq.filter (fun (x, y) -> y = maxY) |> Seq.map fst |> Seq.min
    // let poleMaxX = area.Area.Keys |> Seq.filter (fun (x,y) -> y = maxY) |> Seq.map fst |> Seq.max
    let poleMaxX =
        area.Area.Keys |> Seq.filter (fun (x, y) -> y = 0) |> Seq.map fst |> Seq.max

    let prePoleMaxX = poleMinX - 1
    let prePoleMinX = poleMaxX - poleMinX + 1
    CubeInfo(equatorMinY, equatorMaxY, prePoleMinX, prePoleMaxX, poleMinX, poleMaxX, maxY, maxX)


let rec parseInstructions (s: String) : Inst list =
    if s = "" then
        []
    else
        let chars = s.ToCharArray()

        match chars |> Array.tryFindIndex (fun c -> c < '0' || c > '9') with
        | None -> [ Steps(int s) ]
        | Some (0) -> (if chars[0] = 'L' then Left else Right) :: parseInstructions (s[1..])
        | Some (n) -> Steps(s[0 .. (n - 1)] |> int) :: parseInstructions (s[n..])

let parse (s: string list) : Area * Instructions =
    let area = s |> List.take (s.Length - 2) |> List.indexed

    let cToB (c: char) : Block =
        match c with
        | '.' -> OPEN
        | '#' -> WALL

    let toCord (y: int, s: string) =
        s.ToCharArray()
        |> Array.toList
        |> List.indexed
        |> List.filter (fun (_, c) -> c = '.' || c = '#')
        |> List.map (fun (x, c) -> (x, y), (cToB c))

    let area = area |> List.map toCord |> List.concat |> Map.ofList
    let instructions = s[s.Length - 1] |> parseInstructions |> Instructions
    let south: int = s.Length
    let east = 1 + (s[0 .. s.Length - 2] |> List.map (String.length) |> List.max)
    Area(area, south, east), instructions

let area, instructions =
    File.ReadAllLines "/tmp/aoc/input.t" |> Array.toList |> parse

area |> (printfn "%A")
instructions |> printfn "%A"

printfn $"start {area.StartTile}"

printfn

type State(cubeInfo: CubeInfo, area: Area, pos: Pos, dir: Dir) =
    member this.Area = area
    member this.Pos = pos
    member this.Dir = dir

    member this.ApplyInstruction(inst: Inst) =
        // printfn $"ApplyInstruction {inst}"
        match inst with
        | Left ->
            printfn $"turn to: {turnLeft dir}"
            State(cubeInfo, area, pos, turnLeft dir)
        | Right ->
            printfn $"turn to: {turnRight dir}"
            State(cubeInfo, area, pos, turnRight dir)
        | Steps n ->
            if n = 0 then
                this
            else
                let dir, nextPos = area.NextCubeTile cubeInfo pos dir

                if area.Available nextPos then
                    printfn $"Move to: {nextPos} {dir}"
                    State(cubeInfo, area, nextPos, dir).ApplyInstruction(Steps(n - 1))
                else
                    this

    member this.ApplyCubeInstruction(inst: Inst) =
        // printfn $"ApplyInstruction {inst}"
        match inst with
        | Left ->
            printfn $"turn to: {turnLeft dir}"
            State(cubeInfo, area, pos, turnLeft dir)
        | Right ->
            printfn $"turn to: {turnRight dir}"
            State(cubeInfo, area, pos, turnRight dir)
        | Steps n ->
            if n = 0 then
                this
            else
                let dir, nextPos = area.NextCubeTile cubeInfo pos dir

                if area.Available nextPos then
                    printfn $"Move to: {nextPos} facing:{dir}"
                    State(cubeInfo, area, nextPos, dir).ApplyCubeInstruction(Steps(n - 1))
                else
                    this

    override this.ToString() = $"State ({pos},{dir}"

let initState (area: Area) =
    let cubeInfo = initCubeInfo area
    State(cubeInfo, area, area.StartTile, EAST)

let solve1 (area: Area) (instructions: Instructions) =
    let state = initState area

    let rec solve1 (state: State) (instructions: Instructions) =
        if instructions.Inst.IsEmpty then
            state
        else
            // printfn $"solve1 moving: {state}"
            let state = state.ApplyInstruction(instructions.Inst.Head)
            solve1 state (Instructions(instructions.Inst.Tail))

    solve1 state instructions

let solve2 (area: Area) (instructions: Instructions) =
    let state = initState area

    let rec solve1 (state: State) (instructions: Instructions) =
        if instructions.Inst.IsEmpty then
            state
        else
            // printfn $"solve1 moving: {state}"
            let state = state.ApplyCubeInstruction(instructions.Inst.Head)
            solve1 state (Instructions(instructions.Inst.Tail))

    solve1 state instructions


// let state = solve1 area instructions

// printfn $"state = {state}"

let state = solve2 area instructions

let score (state: State) =
    let x = fst state.Pos + 1
    let y = snd state.Pos + 1
    let dir = state.Dir

    let dirScore =
        match dir with
        | EAST -> 0
        | SOUTH -> 1
        | WEST -> 2
        | NORTH -> 3

    (y * 1000) + (4 * x) + dirScore

let task1 = score state
printfn $"RES 1 {task1}"

let cubeInfo = initCubeInfo area

cubeInfo |> printfn "%A"
