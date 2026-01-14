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
{
    targetUrl = new Uri($"ws://localhost:{port}");
}
else if (!Uri.TryCreate(connectionTarget, UriKind.Absolute, out targetUrl))
{
    Console.WriteLine("Failed to parse URL");
    return 1;
}

if (targetUrl.Scheme != "ws")
{
    Console.WriteLine("Scheme must be ws (websocket)");
    return 1;
}

var link = new LinkInterface();
await link.Connect(targetUrl, CancellationToken.None);

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

// check result
if (query.IsOk)
{
    Console.WriteLine("Success!");
    Console.WriteLine(string.Join('\n', query.ResultValue.ToArray()));
    return 0;
}
else
{
    Console.WriteLine("Error!");
    Console.WriteLine($"Error: {query.ErrorValue.Message}");
    return 1;
}
```

## Items to note

- On demand: It will communicate with ResoniteLink to explore the heigherarchy as it needs to.
- No batching of requests: Each request for data is a separtate request, so queries that require many fetches of new data will have high latency
- Immutable and composabe: when calling a method on the builder a new one is returned istead of modifying the current instance. You can use this to reuse parts of queries to create more complex behavior. 
- .NET Standard 2.0: compatible with .NET and .NET Framework (Unity)

## FSharp API

The underlying API is implemented in F#, and that API can be utilized directly as well. You can consume the library in multiple ways depending on your preference (including using the extension methods like C#).

One notable difference is that with F# API you define queries without providing the LinkInterface until you want to execute the query (called point free style).

```fsharp
// Using the pipe operator
let nodesPipe =
    root
    |> andThen childrenDeep
    |> filter (fun x -> x.Name.Value = "TestSlot")
    |> andThen components
    |> ofType "FrooxEngine.ReferenceField<FrooxEngine.Slot>"
    |> getMember<Reference> "Reference"
    |> andThen dereferenceSlotShallow
    |> andThen ancestorsShallow
    |> map _.Name.Value
    |> toArrayAsync (link.ToInterface())
    
// Some special operators (>>=/bind and >=>/Kleisli) can be used for extra terseness
// The >>= indicates clearly where new data is fetched from the data model
// |> is an in-memory operation
let nodesOperators =
    root
    >>= childrenDeep
    |> filter (fun x -> x.Name.Value = "TestSlot")
    >>= components
    |> ofType "FrooxEngine.ReferenceField<FrooxEngine.Slot>"
    |> getMember<Reference> "Reference"
    >>= dereferenceSlotShallow
    >>= ancestorsShallow
    |> map _.Name.Value
    |> toArrayAsync (link.ToInterface())

// C# style extension methods
let nodesExtensions =
    link
        .RPath()
        .Children()
        .Where(fun x -> x.Name.Value = "TestSlot")
        .Components()
        .OfType("FrooxEngine.ReferenceField<FrooxEngine.Slot>")
        .Member<Reference>("Reference")
        .DereferenceSlot()
        .AncestorsShallow()
        .Select(_.Name.Value)
        .ToListAsync()
```
