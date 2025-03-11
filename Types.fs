namespace Inertial.Giraffe

open Giraffe
open Inertial.Lib
open Microsoft.FSharp.Core
open Newtonsoft.Json
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open FableJson
open System
open System.IO
open System.Threading.Tasks
open Inertial.Lib.Types
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers

module Types =
    
    
    
    type IAsyncBoxer =  
        abstract BoxAsyncResult : obj -> Async<obj>
  
    type AsyncBoxer<'T>() = 
        interface IAsyncBoxer with

            member this.BoxAsyncResult(boxedAsync: obj) : Async<obj> = 
                match boxedAsync with
                // 'T must be when initializing the AsyncBoxer<'T>
                | :? Async<'T> as unboxedAsyncOfGenericValueT -> 
                    async { 
                        // this is of type 'T  
                        let! unwrappedGenericValueTfromAsync  = unboxedAsyncOfGenericValueT
                        // return boxed but still keeping type information 
                        // about 'T in this class
                        return box unwrappedGenericValueTfromAsync
                    }
                | _ -> failwith "Invalid boxed async value"

    type IChoice3Boxer =  
        abstract BoxChoice3Result : obj -> Choice<obj,obj,obj>
        abstract Reboxer : obj -> obj

    type Choice3Boxer<'T,'U,'V>() = 
            
        interface IChoice3Boxer with
            member this.Reboxer (boxedChoice3: obj) =
                match boxedChoice3 with
                // 'T,'U,'V must be when initialized with the interface
                | :? Choice<obj,obj,obj> as unboxedChoice3OfGenericValuesTUV -> 
                    let unwrappedGenericValueTUVfromChoice3  =
                        let newChoice3 : Choice<'T,'U,'V> =
                            match unboxedChoice3OfGenericValuesTUV with
                            | Choice1Of3 c1 -> Choice1Of3 (c1 :?> 'T)
                            | Choice2Of3 c2 -> Choice2Of3 (c2 :?> 'U)
                            | Choice3Of3 c3 -> Choice3Of3 (c3 :?> 'V)
                        // return boxed but still keeping type information 
                        // about 'T in this class
                        newChoice3
                    unwrappedGenericValueTUVfromChoice3
                | _ -> failwith "Invalid boxed choice3 value"
            
            member this.BoxChoice3Result(boxedChoice3: obj) : Choice<obj,obj,obj> = 
                match boxedChoice3 with
                | :? Choice<'T,'U,'V> as unboxedChoice3OfGenericValuesTUV -> 
                    let unwrappedGenericValueTUVfromChoice3  =
                        match unboxedChoice3OfGenericValuesTUV with
                        | Choice1Of3 c1 -> Choice1Of3 (box c1)
                        | Choice2Of3 c2 -> Choice2Of3 (box c2)
                        | Choice3Of3 c3 -> Choice3Of3 (box c3)
                        // return boxed but still keeping type information 
                        // about 'T in this class
                    unwrappedGenericValueTUVfromChoice3
                | _ -> failwith "Invalid boxed choice3 value"
    
    type IChoice2Boxer =  
        abstract BoxChoice2Result : obj -> Choice<obj,obj>
        //abstract BoxAsyncDataResult : obj -> AsyncDataName -> Choice<obj,obj>
        abstract Reboxer : obj -> obj
        
        abstract ReboxToAsyncData : obj -> AsyncDataName -> obj
    
    type Choice2Boxer<'T,'U>() = 
            
        interface IChoice2Boxer with
            
            
            member this.Reboxer (boxedChoice2: obj) =
                match boxedChoice2 with
                // 'T,'U must be when initialized with the interface
                | :? Choice<obj,obj> as unboxedChoice2OfGenericValuesTUV -> 
                    let unwrappedGenericValueTUVfromChoice2  =
                        let newChoice2 : Choice<'T,'U> =
                            match unboxedChoice2OfGenericValuesTUV with
                            | Choice1Of2 c1 -> Choice1Of2 (c1 :?> 'T)
                            | Choice2Of2 c2 -> Choice2Of2 (c2 :?> 'U)
                        // return boxed but still keeping type information 
                        // about 'T in this class
                        newChoice2
                    unwrappedGenericValueTUVfromChoice2
                | _ -> failwith "Invalid boxed choice2 value"
                
            member this.ReboxToAsyncData (boxedChoice2: obj) (asyncDataType : AsyncDataName) : obj =
                match boxedChoice2 with
                // 'T,'U must be when initialized with the interface
                | :? Choice<obj,obj> as unboxedChoice2OfGenericValuesTUV -> 
                    let returnValue  =
                        let newChoice =
                            match unboxedChoice2OfGenericValuesTUV with
                            | Choice1Of2 c1 -> Choice1Of2 (c1 :?> 'T)
                            | Choice2Of2 c2 -> Choice2Of2 (c2 :?> 'U)
                        
                        // return boxed but still keeping type information 
                        // about 'T in this class

                        let innerType =
                            match asyncDataType with
                            | AsyncDataName.Choice2List | AsyncDataName.Choice2Option ->
                                newChoice.GetType().GetGenericArguments().[0].GetGenericArguments().[0].GetGenericArguments().[0]
                            | AsyncDataName.Choice2OptionList | AsyncDataName.Choice2ResultList ->
                                newChoice.GetType().GetGenericArguments().[0].GetGenericArguments().[0].GetGenericArguments().[0].GetGenericArguments().[0]
                                                
                        let asyncDataResultT = typedefof<AsyncData<_>>.MakeGenericType [|innerType|]
                        let expr =
                          Expr.NewUnionCase(
                            FSharpType.GetUnionCases(asyncDataResultT) |>
                                function
                                | [||] -> failwith "Provided input does not have required format"
                                | b -> b
                                       |> Array.pick (
                                                function
                                                | x when x.Name = asyncDataType.ToName() -> Some x
                                                | _ -> None),  [ <@ newChoice @> ])
                        let out = LeafExpressionConverter.EvaluateQuotation expr
                        
                        
                        out
                    returnValue
                | _ -> failwith "Invalid boxed value: {}"
            
            member this.BoxChoice2Result(boxedChoice2: obj) : Choice<obj,obj> = 
                match boxedChoice2 with
                | :? Choice<'T,'U> as unboxedChoice2OfGenericValuesTUV -> 
                    let unwrappedGenericValueTUVfromChoice2  =
                        match unboxedChoice2OfGenericValuesTUV with
                        | Choice1Of2 c1 -> Choice1Of2 (box c1)
                        | Choice2Of2 c2 -> Choice2Of2 (box c2)
                        // return boxed but still keeping type information 
                        // about 'T in this class
                    unwrappedGenericValueTUVfromChoice2
                | _ -> failwith $"Invalid boxed choice2 value {boxedChoice2}"
          
    type IAsyncDataBoxer =  
        //abstract AsyncDataResult : obj -> AsyncData<obj>
        abstract Reboxer : obj -> obj
    
    type AsyncDataBoxer<'T>() = 
            
        interface IAsyncDataBoxer with
            member this.Reboxer (boxedAsyncData: obj) =
                match boxedAsyncData with
                // 'T,'U must be when initialized with the interface
                | :? AsyncData<Async<'T>> as unboxedAsyncDataOfGenericValueT -> 
                    let unwrappedGenericValueTfromAsyncData  =
                        let newAsyncData : AsyncData<'T> =
                            match unboxedAsyncDataOfGenericValueT with
                            | Choice2List c1 -> Choice2List (box c1 :?> AsyncChoice<List<'T>>)
                            | Choice2Option c1 -> Choice2Option (box c1 :?> AsyncChoice<Option<'T>>)
                            | Choice2OptionList c1 -> Choice2OptionList (box c1 :?> AsyncChoice<Option<List<'T>>>)
                            | Choice2ResultList c1 -> Choice2ResultList (box c1 :?> AsyncChoice<Result<List<'T>,string>>)
                            //| Choice2Of2 c2 -> Choice2Of2 (c2 :?> 'U)
                        // return boxed but still keeping type information 
                        // about 'T in this class
                        newAsyncData
                    unwrappedGenericValueTfromAsyncData
                | _ -> failwith "Invalid boxed async data value"
            
            // member this.AsyncDataResult(boxedAsyncData: obj) : AsyncData<obj> = 
            //     match boxedAsyncData with
            //     | :? AsyncData<'T> as unboxedAsyncDataOfGenericValueT -> 
            //         let unwrappedGenericValueTfromAsyncData  =
            //             match unboxedAsyncDataOfGenericValueT with
            //             | Choice2List c1 -> Choice2List (box c1)
            //             //| Choice2Of2 c2 -> Choice2Of2 (box c2)
            //             // return boxed but still keeping type information 
            //             // about 'T in this class
            //         unwrappedGenericValueTfromAsyncData
            //     | _ -> failwith "Invalid boxed async data value"
                        
    /// Determine if the given header is present
    let private hdr (headers : IHeaderDictionary) hdr =
        match headers[hdr] with it when it = StringValues.Empty -> None | it -> Some it[0]

    type IHeaderDictionary with
      
        /// Inertia Request
        member this.Inertial with get () = hdr this "X-Inertial" |> Option.map bool.Parse

        /// Is Inertia SSE Triggered
        member this.InertialSSE with get () = hdr this "X-Inertial-SSE" |> Option.map bool.Parse
        
        /// Is Inertia Reload
        member this.InertialReload with get () = hdr this "X-Inertial-Reload" |> Option.map bool.Parse
        
        /// Inertia Version
        member this.InertialVersion with get () = hdr this "X-Inertial-Version"

        /// Inertia Location
        member this.InertialLocation with get () = hdr this "X-Inertial-Location"

        /// Inertial Partial Data
        member this.InertialPartialData with get () = hdr this "X-Inertial-Partial-Data"
        
        /// Inertial Id from client
        member this.InertialId with get () = hdr this "X-Inertial-Id"
        
        /// Inertial Partial Component
        member this.InertialPartialComponent with get () = hdr this "X-Inertial-Partial-Component"
        
        /// Inertial Full Component
        member this.InertialFullComponent with get () = hdr this "X-Inertial-Full-Component"

        /// Get the token from request set by axios when XSRF-COOKIE is present
        member this.XSRFToken with get () = hdr this "X-XSRF-TOKEN"

        /// Get the referer
        member this.TryGetReferer with get () = hdr this "Referer"
        
        /// Inertial CacheStorage
        member this.InertialCacheStorage with get () = hdr this "X-Inertial-CacheStorage"
        
        /// Inertial CacheRetrieval
        member this.InertialCacheRetrieval with get () = hdr this "X-Inertial-CacheRetrieval"

    /// Extensions for the request object
    type HttpRequest with

        /// Check whether this request was initiated from Inertia
        member this.IsInertial with get () = this.Headers.Inertial |> Option.defaultValue false
        
        /// Check whether this request resulted from an SSE-triggered reload from Inertia
        member this.IsInertialSSE with get () = this.Headers.InertialSSE |> Option.defaultValue false
        
        /// Check whether this request resulted from a Reload from Inertia
        member this.IsInertialReload with get () = this.Headers.InertialReload |> Option.defaultValue false
        
        member this.InertialId with get () = this.Headers.InertialId |> Option.defaultValue (ShortGuid.fromGuid(System.Guid.NewGuid()))
        //member this.InertiaIdOrNew with get () = this.Headers.InertiaId |> Option.defaultValue (Guid.NewGuid().ToString())
            
    type FableRemotingJsonSerializer() =
        interface Giraffe.Json.ISerializer with
            member _.SerializeToString(o: 'T) = JsonConvert.SerializeObject(o,converters=[|fableConverter|])

            member _.SerializeToBytes<'T>(o: 'T): byte array =
                JsonConvert.SerializeObject(o,converters=[|fableConverter|])
                |> System.Text.Encoding.UTF8.GetBytes

            member _.SerializeToStreamAsync (x : 'T) (stream : Stream) = 
                task {
                    use sw = new StreamWriter (stream, jsonEncoding, 1024, true)
                    use writer = new JsonTextWriter (sw, CloseOutput = false)
                    fableSerializer.Serialize (writer, x)
                } :> Task

            member _.Deserialize<'T>(json: string): 'T =
                JsonConvert.DeserializeObject<'T>(json, Settings)

            member _.Deserialize<'T>(bytes: byte []): 'T =
                let json = System.Text.Encoding.UTF8.GetString bytes
                JsonConvert.DeserializeObject<'T>(json, Settings)

            member _.DeserializeAsync(stream: Stream): Task<'T> =
                task {
                    use memoryStream = new MemoryStream()
                    do! stream.CopyToAsync(memoryStream)
                    memoryStream.Seek(0L, SeekOrigin.Begin) |> ignore
                    use streamReader = new StreamReader(memoryStream)
                    use jsonTextReader = new JsonTextReader(streamReader)
                    return fableSerializer.Deserialize<'T>(jsonTextReader)
                }