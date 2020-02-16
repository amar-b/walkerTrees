// Learn more about F# at http://fsharp.org

open System
type Value<'a> = {value:'a; width:float} 
type PosValue<'a> = {value:'a; width:float; x:float; modifier:float}
type tree<'a> =
    | Node of Value<'a> * array<tree<'a>>
    | PosNode of PosValue<'a> * array<tree<'a>>
    | Leaf of Value<'a>
    | PosLeaf of PosValue<'a>

// constants
let siblingSaperation = 4.0
let subtreeSaperation = 4.0
let avgNodeWidth = 2.0

// helpers
let emptyTreeArray = Array.empty<tree<'a>>

let getChildren = function
    | Node(_,c) | PosNode(_,c) -> c
    | _ -> emptyTreeArray;

let treePrelim = function
    | PosNode(v, _) | PosLeaf (v) -> v.x
    | _ -> 0.0

let treeMod = function
    | PosNode(v, _) -> v.modifier
    | _ -> 0.0

let treeVal = function
    | PosNode(v, _) | PosLeaf (v) -> v.value
    | _ -> ""

let prevPrelim arr = 
    match (Array.length arr) with
    | 0 -> 0.0
    | l -> treePrelim arr.[l-1]
         
let childrenPrelim arr = 
    match (Array.length arr) with
    | 0 -> 0.0
    | l -> (treePrelim arr.[0] + treePrelim arr.[l-1]) / 2.0

let getContour node isLeftCountour =
    let mutable countorMap = Map.empty<int, float>;
    let op = if isLeftCountour then min else max;
    let rec contour cn modsum depth =
        let value = modsum + treePrelim cn
        match (Map.tryFind depth countorMap) with
                        | Some prevValue -> countorMap <- Map.add depth (op prevValue value) countorMap
                        | None -> countorMap <- Map.add depth value countorMap
        getChildren cn |> Array.iter (fun n -> (contour n (modsum + treeMod cn) (depth+1)));
    getChildren node |> Array.iter (fun n -> (contour n (treeMod node) 1));
    countorMap|> Map.toArray |> Array.map snd
    
let adjAmount lTreeRContour rTreeLContour =
    seq {0 .. 1 .. (min (Array.length lTreeRContour) (Array.length rTreeLContour))-1 }
        |> Seq.fold (fun shamt i -> 
                        let distance = rTreeLContour.[i] - lTreeRContour.[i];
                        let minDistnce = subtreeSaperation + avgNodeWidth;
                        if (distance < minDistnce) then max (minDistnce-distance) shamt else max distance shamt
                    ) 0.0;
    
let shiftTree gap = function
    | PosLeaf(v) -> PosLeaf({ v with x = v.x + gap; modifier = v.modifier + gap})
    | PosNode(v,c) -> PosNode({ v with x = v.x + gap; modifier = v.modifier + gap}, c )

let shiftTrees trees shamt =
    let len = Array.length trees;
    let gap i = match i with
                        | 0 -> 0.0
                        | l when l = len-1 -> shamt
                        | _ -> shamt/((float)len - 1.0)
    trees |> Array.mapi (fun i v -> shiftTree (gap i) v)
    
let rec firstwalk tree leftSiblings =
    let foldChildren acc e =
        let mutable cache = Array.concat [acc; [|firstwalk e acc|]];
        let rTreeLContour = getContour (Array.last cache) true;
        for i in 0..(Array.length cache - 2) do
            cache <- shiftTrees cache (adjAmount (getContour cache.[i] false) rTreeLContour);
        cache

    match tree with
    | Leaf(v) ->
        let p = match (Array.length leftSiblings) with
                        | 0 -> 0.0
                        | _ ->  siblingSaperation + avgNodeWidth + (prevPrelim leftSiblings);
        PosLeaf({value=v.value; width=v.width; x=p; modifier=0.0})
    | Node(v, arr) ->
        let c = Array.fold foldChildren [|firstwalk arr.[0] emptyTreeArray|] arr.[1..];
        let (x, modifer) = match (Array.length leftSiblings) with
                                | 0 -> (childrenPrelim c, 0.0)
                                | _ -> 
                                    let p = siblingSaperation + avgNodeWidth + (prevPrelim leftSiblings);
                                    (p, p - childrenPrelim c);
        PosNode({value=v.value; width=v.width; x=x; modifier=modifer}, c)

let rec secondwalk parentMod depth tree =
    match tree with
    | PosLeaf(v) -> PosLeaf({v with x=v.x+parentMod;})
    | PosNode(v, arr) -> 
        let modifier = v.modifier + parentMod;
        let c = Array.map (fun n -> secondwalk modifier (depth+1) n) arr
        PosNode({v with x=v.x+parentMod}, c)

let walkerTreePositioner tree = 
    firstwalk tree emptyTreeArray |> secondwalk 0.0 0

// Test tree
let t3 = Node({value="O";width=1.0}, [|
            Node({value="E";width=1.0}, [|
                Leaf({value="A";width=1.0});
                Node({value="D";width=1.0}, [|
                    Leaf({value="B";width=1.0});
                    Leaf({value="C";width=1.0});
                |])
            |]);
            Leaf({value="F";width=1.0});
            Node({value="N";width=1.0}, [|
                Leaf({value="G";width=1.0});
                Node({value="M";width=1.0}, [|
                    Leaf({value="H";width=1.0});
                    Leaf({value="I";width=1.0});
                    Leaf({value="J";width=1.0});
                    Leaf({value="K";width=1.0});
                    Leaf({value="L";width=1.0});
                |])
            |])
        |])
[<EntryPoint>]
let main argv =
    let finalTree = walkerTreePositioner t3
    printf "%A" finalTree
    0
