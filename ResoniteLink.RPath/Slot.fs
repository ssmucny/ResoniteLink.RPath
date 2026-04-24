namespace ResoniteLink.RPath

open System
open ResoniteLink

/// <summary>
/// Axis operations rooted at a single slot. All functions are thin wrappers over
/// <c>Query.wrap slot |&gt; Query.*</c> — refer to the <see cref="Query"/> module for
/// full semantics and remarks on each operation.
/// </summary>
module Slot =

    /// <summary>Gets the direct children of a slot.</summary>
    let children (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot) |> Query.children includeComponents

    /// <summary>Gets direct children of a slot that satisfy a predicate.</summary>
    let inline child ([<InlineIfLambda>] childPredicate) (includeComponents: bool) (slot: Slot) : Query<Slot> =
        slot |> children includeComponents |> Query.filter childPredicate

    /// <summary>Gets the direct children of a slot without component data.</summary>
    let childrenLite (slot: Slot) : Query<Slot> = children false slot

    /// <summary>Gets the direct children of a slot with full component data.</summary>
    let childrenFull (slot: Slot) : Query<Slot> = children true slot

    /// <summary>Gets all descendants of a slot (children, grandchildren, etc.).</summary>
    let descendants (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot)
|> Query.descendants includeComponents

    /// <summary>Gets all descendants of a slot without component data.</summary>
    let descendantsLite (slot: Slot) : Query<Slot> = descendants false slot

    /// <summary>Gets all descendants of a slot with full component data.</summary>
    let descendantsFull (slot: Slot) : Query<Slot> = descendants true slot

    /// <summary>Gets the slot and all of its descendants (children, grandchildren, etc.).</summary>
    let descendantsAndSelf (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot)
        |> Query.descendantsAndSelf includeComponents

    /// <summary>Gets the slot and all of its descendants without component data.</summary>
    let descendantsAndSelfLite (slot: Slot) : Query<Slot> = descendantsAndSelf false slot

    /// <summary>Gets the slot and all of its descendants with full component data.</summary>
    let descendantsAndSelfFull (slot: Slot) : Query<Slot> = descendantsAndSelf true slot

    /// <summary>Gets the parent of a slot.</summary>
    let parent (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot) |> Query.parent includeComponents

    /// <summary>Gets the parent of a slot without component data.</summary>
    let parentLite (slot: Slot) : Query<Slot> = parent false slot

    /// <summary>Gets the parent of a slot with full component data.</summary>
    let parentFull (slot: Slot) : Query<Slot> = parent true slot

    /// <summary>Gets all ancestors of a slot (parent, grandparent, etc.) up to the root.</summary>
    let ancestors (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot)
        |> Query.ancestors includeComponents

    /// <summary>Gets all ancestors of a slot without component data.</summary>
    let ancestorsLite (slot: Slot) : Query<Slot> = ancestors false slot

    /// <summary>Gets all ancestors of a slot with full component data.</summary>
    let ancestorsFull (slot: Slot) : Query<Slot> = ancestors true slot

    /// <summary>Gets the slot and all of its ancestors (parent, grandparent, etc.) up to the root.</summary>
    let ancestorsAndSelf (includeComponents: bool) (slot: Slot) : Query<Slot> =
        Query.wrap (nullArgCheck (nameof slot) slot)
        |> Query.ancestorsAndSelf includeComponents

    /// <summary>Gets the slot and all of its ancestors without component data.</summary>
    let ancestorsAndSelfLite (slot: Slot) : Query<Slot> = ancestorsAndSelf false slot

    /// <summary>Gets the slot and all of its ancestors with full component data.</summary>
    let ancestorsAndSelfFull (slot: Slot) : Query<Slot> = ancestorsAndSelf true slot

    let private normalizeFreshId (id: string) =
        if isNull id then
            id
        elif id.StartsWith Reso.FreshIdPrefix then
            id.Substring Reso.FreshIdPrefix.Length
        else
            id

    let private parentIdOrThrow (slot: Slot) =
        if isNull slot || isNull slot.Parent || isNull slot.Parent.TargetID then
            invalidArg (nameof slot) "slot.Parent.TargetID is required for patch operations."

        slot.Parent.TargetID

    /// Create a shallow copy without children/components because API operations are flat.
    let private copySlotLite (slot: Slot) : Slot =
        Slot(
            ID = slot.ID,
            Parent = slot.Parent,
            Name = slot.Name,
            IsActive = slot.IsActive,
            IsPersistent = slot.IsPersistent,
            Tag = slot.Tag,
            Position = slot.Position,
            Rotation = slot.Rotation,
            Scale = slot.Scale,
            OrderOffset = slot.OrderOffset
        )

    let rec private flattenNodeDirect (parentId: string) (slot: Slot) : seq<DataModelOperation> =
        seq {
            slot.Parent <- Reference(TargetID = parentId)

            if isNull slot.ID then
                slot.ID <- Guid.NewGuid().ToString()

            yield AddSlot(Data = copySlotLite slot)

            if not (isNull slot.Components) then
                for comp in slot.Components do
                    if isNull comp.ID then
                        comp.ID <- Guid.NewGuid().ToString()

                    yield AddComponent(Data = comp, ContainerSlotId = slot.ID)

            if not (isNull slot.Children) then
                for child in slot.Children do
                    yield! flattenNodeDirect slot.ID child
        }

    let rec private flattenNode (parentId: string) (slot: Slot) : seq<DataModelOperation> =
        assert (isNull parentId |> not)

        seq {
            slot.Parent <- Reference(TargetID = normalizeFreshId parentId)

            if isNull slot.ID then
                slot.ID <- Guid.NewGuid().ToString()
                yield AddSlot(Data = copySlotLite slot)
            elif slot.ID.StartsWith Reso.FreshIdPrefix then
                slot.ID <- slot.ID.Substring Reso.FreshIdPrefix.Length
                yield AddSlot(Data = copySlotLite slot)
            else
                yield UpdateSlot(Data = copySlotLite slot)

            if not (isNull slot.Components) then
                for comp in slot.Components do
                    if isNull comp.ID then
                        yield AddComponent(Data = comp, ContainerSlotId = slot.ID)
                    elif comp.ID.StartsWith Reso.FreshIdPrefix then
                        comp.ID <- comp.ID.Substring Reso.FreshIdPrefix.Length
                        yield AddComponent(Data = comp, ContainerSlotId = slot.ID)
                    else
                        yield UpdateComponent(Data = comp)

            if not (isNull slot.Children) then
                for child in slot.Children do
                    yield! flattenNode slot.ID child
        }

    /// <summary>
    /// Flattens a slot tree into Add operations under the supplied parent ID.
    /// Sets the ID of slots/components to IDs that will reflect their position in the data model.
    /// </summary>
    /// <remarks>Does not check for pre-existing slots. Only use if ALL slots/components should be added.</remarks>
    let addUnder (parentId: string) (slot: Slot) : ResizeArray<DataModelOperation> =
        flattenNodeDirect parentId slot |> ResizeArray

    /// <summary>
    /// Flattens multiple slot trees into Add operations under the supplied parent ID.
    /// Sets the ID of slots/components to IDs that will reflect their position in the data model.
    /// </summary>
    /// <remarks>Does not check for pre-existing slots. Only use if ALL slots/components should be added.</remarks>
    let addSlotsUnder (parentId: string) (slots: #seq<Slot>) : ResizeArray<DataModelOperation> =
        slots |> Seq.collect (flattenNodeDirect parentId) |> ResizeArray

    /// <summary>
    /// Flattens a slot tree into Add/Update operations based on ID semantics.
    /// Sets the ID of slots/components to IDs that will reflect their position in the data model.
    /// </summary>
    let patch (slot: Slot) : ResizeArray<DataModelOperation> =
        flattenNode (parentIdOrThrow slot) slot |> ResizeArray

    /// <summary>
    /// Flattens multiple slot trees into Add/Update operations based on ID semantics.
    /// Sets the ID of slots/components to IDs that will reflect their position in the data model.
    /// </summary>
    let patchSlots (slots: #seq<Slot>) : ResizeArray<DataModelOperation> =
        slots
        |> Seq.collect (fun slot -> flattenNode (parentIdOrThrow slot) slot)
        |> ResizeArray
