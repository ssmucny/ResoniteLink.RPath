# RPath

A query EDSL that allows querying ResoniteLink with more ergonomic operations. Similar in style to [LINQ to XML](https://learn.microsoft.com/en-us/dotnet/standard/linq/linq-xml-overview) with some differencecs.

Usage from C# follows a similar pattern to LINQ where you utilize a special type (called `RPathBuilder<T>` here), and you perform operations by calling extension methods defined on the type. They can be chained together and composed to form more complex queries.

```csharp
using ResoniteLink;
using ResoniteLink.RPath;

// Setup (same as normal)
Console.Write("Connect to (localhost port or ws:// URL): ");
var connectionTarget = Console.ReadLine().Trim();
Uri targetUrl;

if (int.TryParse(connectionTarget, out var port))
    targetUrl = new Uri($"ws://localhost:{port}");
else if (!Uri.TryCreate(connectionTarget, UriKind.Absolute, out targetUrl))
{
    Console.WriteLine("Failed to parse URL");
    return 1;
}

if(targetUrl.Scheme != "ws")
{
    Console.WriteLine("Scheme must be ws (websocket)");
    return 1;
}

// build and execute query
var query = await link.RPath() // start at root
    .Children() // get direct children (egarly fetch component data too)
    .Where(x => x.Name.Value == "TestSlot") // filter children with this name
    .Components() // get the components for the filtered children
    .OfType("FrooxEngine.ReferenceField<FrooxEngine.Slot>") // filter components with the right type
    .Member<Reference>("Reference") // get the Reference field and cast to correct type
    .DereferenceSlot() // Follow Reference to the slot it points to
    .AncestorsShallow() // Get all the parents up to root (no component data)
    .Select(x => x.Name.Value) // Get the names from the slots
    // .ToListAsync(); // throws exception on error instead of packaging as a result
    .ToResultAsync();

if (query.IsOk)
{
    Console.WriteLine("Success!");
    Console.WriteLine(string.Join('\n', query.ResultValue.ToArray()));
}
else
{
    Console.WriteLine("Error!");
    Console.WriteLine($"Error: {query.ErrorValue.Message}");
}
```

## Items to note

- On demand: It will communicate with ResoniteLink to expore the heigherarchy as it needs to.
- No batching of requests: Each request for data is a separtate request, so queries that require many fetches of new data will have high latency
- Immutable and composabe: when calling a method on the builder a new one is returned istead of modifying the current instance. You can use this to reuse parts of queries to create more complex behavior. 

## FSharp API

The underlying API is implemented in F#, and that API can be utilized directly as well. You can consume the library in multiple ways depending on your preference (including using the extension methods like C#).

One notable difference is that with F# API you define queries without providing the LinkInterface until you want to execute the query (called point free style).

```fsharp
// Examples creating the query from above using the F# API

// Using the pipe operator
let nodesPipes: Task<string[]> =
    root
    |> andThen childrenDeep
    |> filter (fun x -> x.Name.Value = "TestSlot")
    |> andThen components
    |> ofType "FrooxEngine.ReferenceField<FrooxEngine.Slot>"
    |> getMember<Reference> "Reference"
    |> dereferenceSlotShallow
    |> andThen ancestorsShallow
    |> map _.Name.Value
    |> toArrayAsync (linkObject.ToInterface())
    
// Some special opeators (>>=/bind and >=>/Kleisli) can be used for extra terseness
let nodesOperators: Task<string[]> =
    root
    >>= childrenDeep
    |> filter (fun x -> x.Name.Value = "TestSlot")
    >>= components
    |> ofType "FrooxEngine.ReferenceField<FrooxEngine.Slot>"
    |> getMember<Reference> "Reference"
    |> dereferenceSlotShallow
    >>= ancestorsShallow
    |> map _.Name.Value
    |> toArrayAsync (linkObject.ToInterface())

// C# style extension methods
let nodesExtensions: Task<System.Collections.Generic.List<string>> =
    linkObject
        .RPath()
        .Children()
        .Where(fun x -> x.Name.Value = "TestSlot")
        .Components()
        .OfType("FrooxEngine.ReferenceField<FrooxEngine.Slot>")
        .Member<Reference>("Reference")
        .DereferenceSlot()
        .Ancestors()
        .Select(_.Name.Value)
        .ToListAsync()
```
