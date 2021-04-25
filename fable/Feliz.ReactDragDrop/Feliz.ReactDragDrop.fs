module Feliz.ReactDragDrop

open Feliz
open Fable.Core.JsInterop
open Fable.Core

let dragDropContext: obj = import "DragDropContext" "react-beautiful-dnd"
let droppable: obj = import "Droppable" "react-beautiful-dnd"
let draggable: obj = import "Draggable" "react-beautiful-dnd"

let splitChildProps props =
    let (children, props) =
        props
        |> unbox<(string*obj) seq>
        |> Seq.toArray
        |> Array.partition (fun (key,_) -> key = "children")

    let children = children |> Array.tryLast |> Option.map snd |> Option.toObj

    {| Props = props; Children = children |}

[<Emit "Object.entries($0)">]
let objectEntries (x: obj) : (string * obj) array = jsNative

let reOrder (items: 'T array) sourceIndex destinationIndex =
    let mutableList = ResizeArray items
    let item = items.[sourceIndex]
    mutableList.RemoveAt(sourceIndex)
    mutableList.Insert(destinationIndex, item)
    mutableList |> Seq.toArray

type DragDropContext =

    static member inline onDragEnd (f) = prop.custom("onDragEnd",f)

    static member inline create (props : IReactProperty seq) =
        let elements = splitChildProps props
        Interop.reactApi.createElement(dragDropContext, createObj !!elements.Props, !!elements.Children)

type Droppable =

    static member inline droppableId (id: string) = prop.custom("droppableId", id)
    static member inline isCombinedEnabled (b: bool) = prop.custom("isCombinedEnabled", b)
    static member inline droppableType (s: string) = prop.custom("type", s)
    static member inline direction (s: string) = prop.custom("direction", s)

    static member inline children (f) = prop.custom("children", f)

    static member spread (value: obj) = objectEntries value |> unbox

    static member inline create (props : IReactProperty seq) =
        let elements = splitChildProps props
        Interop.reactApi.createElement(droppable, createObj !!elements.Props, !!elements.Children)

let (....) v = objectEntries v |> unbox

type Draggable =

    static member inline draggableId (id: string) = prop.custom("draggableId", id)
    static member inline index (i: int) = prop.custom("index", i)

    static member inline key (k: string) = prop.custom("key", k)


    static member inline children (f) = prop.custom("children", f)

    static member inline spread(value: obj) = objectEntries value |> unbox

    static member inline create (props : IReactProperty seq) =
        let elements = splitChildProps props
        Interop.reactApi.createElement(draggable, createObj !!elements.Props, !!elements.Children)