# RPath

RPath is a composable query + builder EDSL for [ResoniteLink](https://github.com/Yellow-Dog-Man/ResoniteLink).
Featuring:

- A lazy `Query<'T>` pipeline for traversing and shaping data model results.
- A declarative builder API (`Reso` + `Slot`) for creating or patching slot/component graphs.
- F#-native combinators and C#-friendly extension methods.

## Package

- NuGet package: `Papaltine.ResoniteLink.RPath`
- Compatible with `.NET Standard 2.0`

```bash
dotnet add package Papaltine.ResoniteLink.RPath --version 0.3.0
```

## Quick start (C#)

Add the following namespaces:

```csharp
using ResoniteLink;
using ResoniteLink.RPath;        // Query<T>, traversal, execution
using ResoniteLink.RPath.CSharp; // Where/Select/Bind/SelectMany
```

Then build a query from `Query.Root` and execute with your `LinkInterface`:

```csharp
LinkInterface link = new LinkInterface();
await link.Connect(new Uri("ws://localhost:12345"), CancellationToken.None);

var proxyUnderRoot = await Query.Root
    .Children(includeComponents: false)
    .Where(slot => slot.Name.Value.Contains("Proxy"))
    .FirstOr(null, link);
    // Use .ToList(link) for multiple results

var slotToUpdate = proxyUnderRoot!;
slotToUpdate.Name.Value = "New Name For Proxy";
var patchResult = await slotToUpdate.Patch(link);
```

## Query Structure

`Query<T>` values are immutable and lazy:

- Building a query does not contact the websocket.
- Executing a query requires a `LinkInterface`.
- Queries are reusable. Build larger queries from several smaller ones.

## API Surface

| Surface | Namespace | Primary use                                                  |
|---|---|--------------------------------------------------------------|
| `Query` module | `ResoniteLink.RPath.Query` | F# combinators and root entry points                         |
| `Operators` module (`>>=`, `>=>`) | `ResoniteLink.RPath.Operators` | F# monadic composition                                       |
| F# extension methods | `ResoniteLink.RPath` | Method chaining in F# and C#                                 |
| C# LINQ-style extensions (`Select`, `Where`, `Bind`, ...) | `ResoniteLink.RPath.CSharp` | Idiomatic C# query style                                     |
| `Slot` module wrappers | `ResoniteLink.RPath.Slot` | Slot-rooted traversal and data model operation helpers       |
| `Reso` constructors | `ResoniteLink.RPath` | Declarative slot/component/member construction               |

## Namespaces

### F#

```fsharp
open ResoniteLink
open ResoniteLink.RPath
open ResoniteLink.RPath.Query      // use query combinators without Query. prefix
open ResoniteLink.RPath.Operators  // >>= and >=>
```

### C#

```csharp
using ResoniteLink;
using ResoniteLink.RPath;
using ResoniteLink.RPath.CSharp;
```

## Query API

### Common entry points

- `Query.root` / `Query.Root`
- `Query.findSlotByID` / `Query.FindSlotByID`
- `Query.findComponentByID` / `Query.FindComponentByID`

### Quick start (F#)

```fsharp
task {
    let link = new LinkInterface();
    do! link.Connect(Uri("ws://localhost:12661"), Threading.CancellationToken.None);

    let! proxyUnderRoot =
        Query.root
        |> Query.children false
        |> Query.filter _.Name.Value.Contains("Proxy")
        |> Query.first
        |> link
        // Use Query.toArray link for multiple results

    match proxyUnderRoot with
    | None -> failwith "No proxy found under root slot."
    | Some proxy ->
        proxy.Name.Value <- "New Name For Proxy"
        let! patchResult = proxy |> Slot.patch |> link.RunDataModelOperationBatch
        ()
}
```

### Traversal

For `Query<Slot>`:

- `children includeComponents` / `childrenLite` / `childrenFull`
- `parent includeComponents` / `parentLite` / `parentFull`
- `ancestors includeComponents` / `ancestorsLite` / `ancestorsFull`
- `descendants includeComponents` / `descendantsLite` / `descendantsFull`
- `ancestorsAndSelf includeComponents` / `ancestorsAndSelfLite` / `ancestorsAndSelfFull`
- `descendantsAndSelf includeComponents` / `descendantsAndSelfLite` / `descendantsAndSelfFull`

Extension methods expose `.GetChildren(...)`, `.Parent(...)`, `.Ancestors(...)`, `.Descendants(...)`,
`.AncestorsAndSelf(...)`, and `.DescendantsAndSelf(...)`.

### Components, members, references

- `Query.components` / `.Components()`
- `Query.ofType` / `.OfType(typeName)`
- `Query.getMember<'T>` / `.Member<T>(memberName)`
- `Query.dereferenceSlot`, `dereferenceSlotLite`, `dereferenceSlotFull` / `.DereferenceSlot(...)`
- `Query.dereferenceComponent` / `.DereferenceComponent()`

`getMember<'T>` / `Member<T>` skips components with missing or incompatible member types.

### Composition and shaping

Query-producing composition:

- `bind`
- `Operators.(>>=)`
- `Operators.(>=>)`

In-memory shaping:

- `map`, `filter`, `flatMap`, `mapAll`, `take`, `skip`, `slice`

`bind` can trigger additional requests per bound execution; use axis combinators (`children`, `descendants`, etc.) when batching behavior is desired.

### Execution and error handling

Execution functions:

- `runAsync` / `.RunAsync(link)`
- `run` / `.Run(link)`
- `toArray` / `.ToArray(link)`
- `toResizeArray` / `.ToList(link)`
- `first` / `.First(link)`
- `firstOr` / `.FirstOr(defaultValue, link)`
- `toResult` / `.ToResult(link)`

`toResult` / `ToResult` converts `ResoniteLinkException` into `Result.Error`. Exceptions thrown inside your own filters/projections still propagate.

## Builder API

Builder APIs let you build declarative trees (`Reso`) and then emit data model operations (`Slot`).

### Constructors (`Reso`)

Use `Reso` to build members, components, and slots:

- Scalar/value members: `Reso.bool`, `Reso.str`, `Reso.int`, `Reso.float`, `Reso.double`, ...
- Vector/quaternion members: `Reso.float2/3/4`, `Reso.double2/3/4`, `Reso.int2/3/4`, `Reso.long2/3/4`, `Reso.floatQ`
- References: `Reso.reference(targetId)`
- Components: `Reso.comp (ID = ..., ComponentType = ...) [ key, member ]`
- Slots: `Reso.slot (...) [ components ] [ children ]`

### Operation emission (`Slot` module)

- `Slot.addUnder parentId slot`
- `Slot.addSlotsUnder parentId slots`
- `Slot.patch slot`
- `Slot.patchSlots slots`

These functions return `ResizeArray<DataModelOperation>` for `link.RunDataModelOperationBatch`.

### Add vs patch matrix

| Function | Input | Emits | Intended use                                                 |
|---|---|---|--------------------------------------------------------------|
| `addUnder` | One tree + parent ID | `AddSlot` + `AddComponent` only | Brand-new slot under known parent                            |
| `addSlotsUnder` | Many trees + parent ID | `AddSlot` + `AddComponent` only | Batch create multiple slots under one parent                 |
| `patch` | One tree with valid `slot.Parent.TargetID` | Mix of `Add*` and `Update*` | Update existing slot and optionally add new slots/components |
| `patchSlots` | Many trees, each with valid parent reference | Mix of `Add*` and `Update*` | Batch patch multiple slots                                   |

> [!WARNING]
> `addUnder` / `addSlotsUnder` never emit updates. If you point them at existing IDs, you can create conflicts.
> Use `patch` / `patchSlots` for updates.

> [!NOTE]
> Delete operations must be emitted manually. `patch` will ignore will only add/update.

### ID semantics in patch workflows

Patch behavior is ID-driven:

- `ID = null` generates and sets a new ID with an add operation
- `ID`s from `Reso.newID()` generate an add operation (use to link IDs without round trips)
- Existing ID causes an update operation

`Slot.patch` and `Slot.patchSlots` require each root slot to have `slot.Parent.TargetID` set. If you used one of the add functions to create the slot, you already have this information. If you built the tree from scratch, you need to set it manually before patching.

### Builder workflow: create new tree

```fsharp
task {
    let tree =
        Reso.slot (Name = "Dashboard", Tag = "ui")
            [ Reso.comp (ComponentType = "[FrooxEngine]FrooxEngine.MeshRenderer")
                [ "SortingOrder", Reso.int 10 ] ]
            [ Reso.slot (Name = "Child") [] [] ]

    let addOps = Slot.addUnder Slot.ROOT_SLOT_ID tree
    let! addResult = link.RunDataModelOperationBatch addOps
    return addResult
}
```

### Builder workflow: patch existing tree + add new nodes

```fsharp
task {
    // Assume tree is already in the model and has Parent.TargetID set.
    tree.Name.Value <- "Dashboard (Patched)"

    // Existing component -> UpdateComponent
    tree.Components[0].Members["SortingOrder"] <- Reso.int 20

    // Fresh component -> AddComponent
    tree.Components.Add(
        Reso.comp (ComponentType = "[FrooxEngine]FrooxEngine.UnlitMaterial") []
    )

    // Fresh child slot -> AddSlot
    tree.Children.Add(Reso.slot (Name = "Patch Child") [] [])

    let patchOps = Slot.patch tree
    let! patchResult = link.RunDataModelOperationBatch patchOps
    return patchResult
}
```

### Builder workflow: add multiple trees in one batch

```fsharp
let slotsToCreate =
    [ Reso.slot (Name = "A") [] []
      Reso.slot (Name = "B") [] [] ]

let addOps = Slot.addSlotsUnder Slot.ROOT_SLOT_ID slotsToCreate
let result = link.RunDataModelOperationBatch addOps
```

### Builder workflow: batch complex batches together

```fsharp
let slotId = Reso.newID()
let batch1 =
    [ Reso.slot (ID = slotId, Name = "Batch1 Slot1") [] []
      Reso.slot (Name = "Batch1 Slot2") [] [] ]

let batch2 =
    [ Reso.slot (Name = "Batch2 Slot1") [] []
      Reso.slot (Name = "Batch2 Slot2") [] [] ]

let allOps =
    seq {
        yield! Slot.addSlotsUnder Slot.ROOT_SLOT_ID batch1
        yield! Slot.addSlotsUnder slotId batch2
    }

let result = link.RunDataModelOperationBatch allOps
```
