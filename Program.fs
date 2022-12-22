open System.IO
open System

let inputFile = "/tmp/aoc/output"

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

type CubeInfo(equatorMinY, equatorMaxY, prePoleMinX, prePoleMaxX, poleMinX, poleMaxX, maxY: int, maxX: int) =
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

    member this.NextCubeTile (cubeInfo: CubeInfo) ((x, y): Pos) (dir: Dir) : Dir * Pos =
        if area.ContainsKey(nextPos dir (x, y)) then
            dir, nextPos dir (x, y)
        else
            let orig = (x, y)

            match dir with
            | EAST when cubeInfo.EquatorMinY > y -> // OK
                let y = cubeInfo.maxY - y
                let x = cubeInfo.maxX
                let dir = WEST
                dir, (x, y)
            | EAST when cubeInfo.EquatorMaxY < y -> // OK
                let x = cubeInfo.PoleMaxX
                let y = cubeInfo.maxY - y
                let dir = WEST
                dir, (x, y)
            | EAST -> // OK
                let x = cubeInfo.EquatorMaxY - y + cubeInfo.PoleMaxX + 1
                let y = cubeInfo.EquatorMaxY + 1
                let dir = SOUTH
                dir, (x, y)
            | WEST when y < cubeInfo.EquatorMinY -> // OK
                let x = cubeInfo.PrePoleMinX + y
                let y = cubeInfo.EquatorMinY
                let dir = SOUTH
                dir, (x, y)
            | WEST when y > cubeInfo.EquatorMaxY -> // OK
                let x = cubeInfo.maxY - y + cubeInfo.PrePoleMinX
                let y = cubeInfo.EquatorMaxY
                let dir = NORTH
                dir, (x, y)
            | WEST -> // OK (?) /// TODO VERIFY
                let x = cubeInfo.maxX - (y - cubeInfo.EquatorMinY)
                let y = cubeInfo.maxY
                let dir = NORTH
                dir, (x, y)
            | SOUTH when x < cubeInfo.PrePoleMinX -> // OK
                let x = cubeInfo.PoleMaxX - x
                let y = cubeInfo.maxY
                let dir = NORTH
                dir, (x, y)
            | SOUTH when x < cubeInfo.PoleMinX -> // OK
                let y = cubeInfo.EquatorMaxY + (cubeInfo.PoleMinX - x)
                let x = cubeInfo.PoleMinX
                let dir = EAST
                dir, (x, y)
            | SOUTH when x > cubeInfo.PoleMaxX -> // OK
                let y = cubeInfo.EquatorMinY + cubeInfo.maxX - x
                let x = 0
                let dir = EAST
                dir, (x, y)
            | SOUTH -> // OK
                let x = cubeInfo.PoleMaxX - x
                let y = cubeInfo.EquatorMaxY
                let dir = NORTH
                dir, (x, y)
            | NORTH when x < cubeInfo.PrePoleMinX -> // OK
                let x = cubeInfo.PoleMaxX - x
                let y = 0
                let dir = SOUTH
                dir, (x, y)
            | NORTH when x < cubeInfo.PoleMinX -> // OK
                let y = x - cubeInfo.PrePoleMinX
                let x = cubeInfo.PoleMinX
                let dir = EAST
                dir, (x, y)
            | NORTH when x > cubeInfo.PoleMaxX ->
                let y = cubeInfo.maxX - x + cubeInfo.EquatorMinY
                let x = cubeInfo.PoleMaxX
                let dir = WEST
                dir, (x, y)
            | NORTH ->
                let x = cubeInfo.PoleMaxX - x
                let y = cubeInfo.EquatorMinY
                let dir = SOUTH
                dir, (x, y)



    override this.ToString() = $"Area({area})"


let initCubeInfo (area: Area) =
    let maxY = area.Area.Keys |> Seq.map snd |> Seq.max
    let maxX = area.Area.Keys |> Seq.map fst |> Seq.max

    let equatorMinY =
        area.Area.Keys |> Seq.filter (fun (x, y) -> x = 0) |> Seq.map snd |> Seq.min

    let equatorMaxY =
        area.Area.Keys |> Seq.filter (fun (x, y) -> x = 0) |> Seq.map snd |> Seq.max

    let poleMinX =
        area.Area.Keys |> Seq.filter (fun (x, y) -> y = maxY) |> Seq.map fst |> Seq.min

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

let area, instructions = File.ReadAllLines inputFile |> Array.toList |> parse

area |> (printfn "%A")
instructions |> printfn "%A"

printfn $"start {area.StartTile}"

type State(cubeInfo: CubeInfo, area: Area, pos: Pos, dir: Dir) =
    member this.Area = area
    member this.Pos = pos
    member this.Dir = dir

    member this.ApplyCubeInstruction(inst: Inst) =
        printfn $"ApplyCubeInstruction {inst} {pos} {dir}"

        match inst with
        | Left -> State(cubeInfo, area, pos, turnLeft dir)
        | Right -> State(cubeInfo, area, pos, turnRight dir)
        | Steps n ->
            if n = 0 then
                this
            else
                let nextDir, nextPos = area.NextCubeTile cubeInfo pos dir
                printfn $"  got {pos} {dir} -> {nextPos} {nextDir}"

                if area.Available nextPos then
                    State(cubeInfo, area, nextPos, nextDir).ApplyCubeInstruction(Steps(n - 1))
                else
                    this

    override this.ToString() = $"State ({pos},{dir}"

let initState (area: Area) =
    let cubeInfo = initCubeInfo area
    State(cubeInfo, area, area.StartTile, EAST)

let solve2 (area: Area) (instructions: Instructions) =
    let state = initState area

    let rec solve (state: State) (instructions: Instructions) =
        if instructions.Inst.IsEmpty then
            state
        else
            printfn $"solve moving: {instructions.Inst.Head}  {state}"
            let state = state.ApplyCubeInstruction(instructions.Inst.Head)
            printfn $"  -> {state}"
            solve state (Instructions(instructions.Inst.Tail))

    solve state instructions

let runAll () =
    let state = solve2 area instructions

    printfn $"Final state 2 : {state}"

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

        (y * 1000) + (4 * (x - 50)) + dirScore

    let task1 = score state
    printfn $"RES 1 {task1}"

let cubeInfo = initCubeInfo area

cubeInfo |> printfn "%A"

area.NextCubeTile cubeInfo (100, 149) WEST |> printfn "%A"

// runAll ()

let all =
    area.Area.Keys
    |> Seq.map (fun pos -> [ (pos, NORTH); (pos, SOUTH); (pos, WEST); (pos, EAST) ])
    |> Seq.toList
    |> List.concat

let opposite (dir: Dir) =
    match dir with
    | EAST -> WEST
    | WEST -> EAST
    | NORTH -> SOUTH
    | SOUTH -> NORTH


let checkReflective (area: Area) ((pos, dir): Pos * Dir) =
    let newDir, newPos = area.NextCubeTile cubeInfo pos dir
    let oldDir, oldPos = area.NextCubeTile cubeInfo newPos (opposite newDir)
    let dirChanged = opposite (oldDir) <> dir
    let posChanged = oldPos <> pos

    if dirChanged || posChanged then
        Some(pos, dir, newPos, newDir, oldPos, opposite oldDir)
    else
        None



// checkReflective area (all.Head)

let checkReflectiveAll () =
    let errors =
        all |> List.map (fun pd -> checkReflective area pd) |> List.filter Option.isSome

    errors |> List.map (printfn "%A")

printfn $"{cubeInfo.maxY}"

// checkReflectiveAll ()

// runAll()

// 64 94 NORTH

printfn $"{cubeInfo}"

let calcHackyPos (x: int, y: int, dir: Dir) : Pos * Dir =
    let side =
        match (x, y) with
        | _ when y < 50 -> 'a'
        | _ when y > 99 && x > 149 -> 'b'
        | _ when y < 100 && x > 99 -> 'c'
        | _ when y > 99 && x < 140 -> 'd'
        | _ when y < 100 && x > 49 && x < 100 -> 'e'
        | _ when y < 100 && x < 50 -> 'f'

    printfn $"side={side}"
    let x = x % 50
    let y = y % 50

    let (x, y) =
        match side with
        | 'a' -> (x + 50, y)
        | 'b' -> (150 - x, 50 - y)
        | 'c' -> (x + 50, y + 50)
        | 'd' -> (y + 100, 50 - x)
        | 'e' -> (x + 50, y + 100)
        | 'f' -> (y, 50 - x + 150)
        
    let dir =
        match side with
        | 'a' -> dir
        | 'b' -> dir |> opposite
        | 'c' -> dir
        | 'd' -> dir |> turnLeft
        | 'e' -> dir
        | 'f' -> dir |> turnLeft
        
    (x + 1, y + 1), dir

let hackyPos = calcHackyPos (64, 94, NORTH)
printfn $"hack pos = {hackyPos}"

let hackyScore ((x,y),dir) : int =
    let dirScore =
        match dir with
        | EAST -> 0
        | SOUTH -> 1
        | WEST -> 2
        | NORTH -> 3

    1000*y + 4*x + dirScore
    
let answer2 = hackyScore hackyPos

printfn $"ANSWER 2: {answer2}"

