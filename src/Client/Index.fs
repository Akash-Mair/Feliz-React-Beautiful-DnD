module Index

open Elmish

type Column =
    | Todo
    | InProgress
    | Completed
        with static member Deserialize = function
                | "Todo" -> Todo
                | "InProgress" -> InProgress
                | "Completed" -> Completed
                | x -> failwithf "Invalid column: %s" x

type Model = { Items: (Column * string array) array   }

type Msg =
    | ReorderItems of Model

let items =
    [| Todo, [| "Clean car"; "Take out the bins" |]
       InProgress, [| "Hoover"; "Code" |]
       Completed, [| "Basic React DnD" |] |]

let init () : Model * Cmd<Msg> =
    let model = { Items = items }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ReorderItems model -> model, Cmd.none

open Feliz
open Feliz.ReactDragDrop

//let (|DraggableWithinSameColumn|_|) (model, res, destination) =
//    if (destination.droppableId = res.source.droppableId && res.``type`` <> "column") then
//        let col = destination.droppableId |> Column.Deserialize
//        let colItems =
//            model.Items
//            |> Array.map (fun (colName, items) ->
//                if colName = col then
//                    colName, reOrder items res.source.index destination.index
//                else colName, items)
//        Some { model with Items = colItems }
//    else None

let reOrderItems (model: Model) (res: {| destination: obj; source: {| droppableId: string; index: int |}; ``type``: string |}) =
    match res.destination with
    | null -> model
    | _ ->
        let destination: {| droppableId: string; index: int |} = unbox res.destination
        //match model, res, destination with
        //| DraggableWithinSameColumn x -> x



        if (destination.droppableId = res.source.droppableId && res.``type`` <> "column") then
            let col = destination.droppableId |> Column.Deserialize
            let colItems =
                model.Items
                |> Array.map (fun (colName, items) ->
                    if colName = col then
                        colName, reOrder items res.source.index destination.index
                    else colName, items)
            { model with Items = colItems }

        elif res.``type`` <> "column" then
            let destinationCol = destination.droppableId |> Column.Deserialize
            let sourceCol = res.source.droppableId |> Column.Deserialize

            let newItemOrder =
                model.Items
                |> Array.map (fun (col,items) ->
                    let t = ResizeArray items
                    if col = sourceCol then
                        t.RemoveAt(res.source.index)
                        col, Seq.toArray t
                    elif col = destinationCol then
                        let sourceItems = model.Items |> Array.find(fst >> (=) sourceCol) |> snd
                        t.Insert(destination.index, sourceItems.[res.source.index])
                        col, Seq.toArray t
                    else col, items
                )
            { model with Items = newItemOrder }
        else
            let items = reOrder model.Items res.source.index destination.index
            { model with Items = items }

let view (model: Model) (dispatch: Msg -> unit) =
    DragDropContext.create [
        DragDropContext.onDragEnd (fun res -> (reOrderItems model res) |> ReorderItems |> dispatch )
        prop.children [
            Droppable.create [
                Droppable.droppableId "All"
                Droppable.droppableType "column"
                Droppable.direction "horizontal"
                Droppable.children [
                    fun (p:  {| innerRef: Browser.Types.Element -> unit; droppableProps: obj; placeholder: ReactElement |}) ->
                        Html.div [
                            prop.ref p.innerRef
                            yield!  (<...>) p.droppableProps
                            prop.style [ style.display.flex; style.height (length.vh 100) ]
                            prop.children [
                                yield! model.Items
                                |> Array.mapi (fun i (col, items) ->
                                    Draggable.create [
                                        Draggable.key (string col)
                                        Draggable.index i
                                        Draggable.draggableId (string col)
                                        Draggable.children [
                                            fun (d: {| innerRef: Browser.Types.Element -> unit; draggableProps: obj; dragHandleProps: obj |}) ->
                                                let props: {| style: obj |} = unbox d.draggableProps
                                                Html.div [
                                                    prop.ref d.innerRef
                                                    yield! (<...>) d.draggableProps
                                                    yield! (<...>) d.dragHandleProps
                                                    prop.style [ yield! (<...>) props.style; style.height 300; style.width 250; style.backgroundColor "lightblue"; style.margin 20; style.padding 20 ]
                                                    prop.children [
                                                        Droppable.create [
                                                            Droppable.droppableId (string col)
                                                            Droppable.children [
                                                                fun (c:  {| innerRef: Browser.Types.Element -> unit; droppableProps: obj; placeholder: ReactElement |}) ->
                                                                    Html.div [
                                                                        prop.ref c.innerRef
                                                                        yield! (<...>) c.droppableProps
                                                                        prop.children [
                                                                            yield! items
                                                                            |> Array.mapi (fun i item ->
                                                                                Draggable.create [
                                                                                    Draggable.draggableId item
                                                                                    Draggable.index i
                                                                                    Draggable.key item
                                                                                    Draggable.children [
                                                                                        fun (x: {| innerRef: Browser.Types.Element -> unit; draggableProps: obj; dragHandleProps: obj |}) ->
                                                                                            let props: {| style: obj |} = unbox d.draggableProps
                                                                                            Html.div [
                                                                                                prop.ref x.innerRef
                                                                                                yield! (<...>) x.draggableProps
                                                                                                yield! (<...>) x.dragHandleProps
                                                                                                prop.text item
                                                                                            ]
                                                                                    ]
                                                                                ])
                                                                            c.placeholder
                                                                        ]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        )
                                p.placeholder
                                ]
                            ]
                        ]
                ]
            ]
        ]