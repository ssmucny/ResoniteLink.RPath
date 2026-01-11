namespace ResoniteLink

open System.Threading.Tasks
open ResoniteLink

/// <summary>
/// Interpreter interface that defines the abstract operations available on a link that can interact with the data model.
/// </summary>
/// <remarks>This enables using a tagless final API design to decouple from the specific link implementation.
/// This could be implemented for unit tests with a mock link, or even an in-game Resonite mod.</remarks>
type ILinkInterface =
    abstract member GetSlotData: request: GetSlot -> Task<SlotData>
    abstract member GetComponentData: request: GetComponent -> Task<ComponentData>
    abstract member AddSlot: request: AddSlot -> Task<Response>
    abstract member UpdateSlot: request: UpdateSlot -> Task<Response>
    abstract member RemoveSlot: request: RemoveSlot -> Task<Response>
    abstract member AddComponent: request: AddComponent -> Task<Response>
    abstract member UpdateComponent: request: UpdateComponent -> Task<Response>
    abstract member RemoveComponent: request: RemoveComponent -> Task<Response>
    abstract member ImportTexture: request: ImportTexture2DFile -> Task<AssetData>
    abstract member ImportTexture: request: ImportTexture2DRawData -> Task<AssetData>

module ILinkInterface =
    let ofResoniteLinkInterface (link: ResoniteLink.LinkInterface) =
        // Because LinkInterface does not implement the ILinkInterface we 'implement' it here
        // If the class was converted to implement the interface directly, then this would no longer be necessary
        { new ILinkInterface with
            member this.AddComponent(request) = link.AddComponent request
            member this.AddSlot(request) = link.AddSlot request
            member this.GetComponentData(request) = link.GetComponentData request
            member this.GetSlotData(request) = link.GetSlotData request
            member this.ImportTexture(request: ImportTexture2DFile) : Task<AssetData> = link.ImportTexture request
            member this.ImportTexture(request: ImportTexture2DRawData) : Task<AssetData> = link.ImportTexture request
            member this.RemoveComponent(request) = link.RemoveComponent request
            member this.RemoveSlot(request) = link.RemoveSlot request
            member this.UpdateComponent(request) = link.UpdateComponent request
            member this.UpdateSlot(request) = link.UpdateSlot request }
