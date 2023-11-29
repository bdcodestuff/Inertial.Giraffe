module FsInertia

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open Microsoft.AspNetCore.Antiforgery 
open System.Text.Json
open Giraffe
open Giraffe.ViewEngine

/// Determine if the given header is present
let private hdr (headers : IHeaderDictionary) hdr =
    match headers[hdr] with it when it = StringValues.Empty -> None | it -> Some it[0]

/// Extensions to the header dictionary
type IHeaderDictionary with
  
    /// Inertia Request
    member this.Inertia with get () = hdr this "X-Inertia"

    /// Inertia Version
    member this.InertiaVersion with get () = hdr this "X-Inertia-Version"

    /// Inertia Location
    member this.InertiaLocation with get () = hdr this "X-Inertia-Location"

    /// Inertia Partial Data
    member this.InertiaPartialData with get () = hdr this "X-Inertia-Partial-Data"
    
    /// Inertia Partial Component
    member this.InertiaPartialComponent with get () = hdr this "X-Inertia-Partial-Component"

type Page =
    {
        ``component`` : string
        props : Map<string,obj>
        version : string
        url : string
    }

    member x.toJson () =
        JsonSerializer.Serialize<Page>(x)

module Handlers =

    open FSharpx.Collections
    open FSharp.Reflection
    
    let handleProps (ctx:HttpContext) componentName (props:Map<string,obj>) =
        task {
            // check if partial data request with specified component name
        let isPartialReq, filter =
            match ctx.Request.Headers.InertiaPartialData, ctx.Request.Headers.InertiaPartialComponent with
            | Some partialData, Some comp when comp = componentName ->
                true,
                partialData.Split(',') 
                |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
                |> Array.map (fun x -> x.Trim())
            | _ ->
                false,
                [||]
        let filteredProps =
            match filter with
            | [||] -> props
            | filter ->
                props |> Map.filter (fun x y -> Array.contains x filter)

        let props = 
            match ctx.Items.TryGetValue("InertiaSharedData") with
            | (false, _) -> filteredProps
            | (true, a) ->
                try 
                    let sharedProps = try a :?> Map<string,obj> with exn -> failwith exn.Message
                    Map.union filteredProps sharedProps
                with exn -> failwith exn.Message
        
        let functions, nonFunctions =
            props
            |> Map.partition (fun _ v -> FSharpType.IsFunction(v.GetType()))
        
        
        if isPartialReq then
            // partition function type props into maps of either async or sync functions based on type signature
            let asyncFunctions, syncFunctions =
                functions
                |> Map.partition (fun _ v -> 
                    match v with
                    | :? (unit -> Task<obj>) -> true
                    | :? (unit -> Async<obj>) -> true
                    | b -> false
                )
            
            // convert tasks to async values, separate keys and values
            let asyncKeys, asyncValues =
                asyncFunctions 
                    |> Map.map (fun _ y ->
                        match y with 
                        | :? (unit -> Task<obj>) as f -> f () |> Async.AwaitTask
                        | :? (unit -> Async<obj>) as f -> f ()
                        | b -> failwith $"unable to handle func prop with type: {b.GetType()}")
                        |> Map.toList
                        |> List.fold (fun (keys,values) (k,v) -> ( (k::keys),(v::values) )) ([],[])
            
            // evaluate the non-async functions
            let evaluatedSyncs =
                syncFunctions 
                    |> Map.map (fun _ y ->
                        match y with 
                        | :? (unit -> obj) as f -> f ()
                        | b -> failwith $"unable to handle func prop with type: {b.GetType()}" )
            
            // evaluate all the async functions in parallel
            let! asyncFuncValues = Async.Parallel asyncValues

            // zip the keys back to the values and convert back to a Map structure
            let evaluatedAsyncs = 
                asyncFuncValues 
                |> List.ofArray
                |> List.zip asyncKeys
                |> Map.ofList

            // merge the evaluated async and sync function maps
            let evaluatedMerged =
                Map.union evaluatedAsyncs evaluatedSyncs

            // return the merged evaluated functions and non-functions
            return Map.union nonFunctions evaluatedMerged

        else 
            // ignore all function type props on full page loads
            return nonFunctions
        }
        

    let setCsrfCookie : HttpHandler =
        fun next ctx -> 
            let tokenSet = ctx.GetService<IAntiforgery>().GetTokens(ctx)
            let options = new CookieOptions()
            options.SameSite <- SameSiteMode.Strict
            ctx.Response.Cookies.Append("XSRF-TOKEN",tokenSet.CookieToken,options)
            next ctx

    let generatePage (nextHandler: Page -> HttpHandler) ctx componentName (props:Map<string,obj>) (url:string option) version : HttpHandler =
        fun next ctx -> 
            task {
                let! evaluatedProps = handleProps ctx componentName props
                let page =
                    {
                        ``component`` = componentName
                        props = evaluatedProps
                        version = version
                        url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
                    }
                return! (nextHandler page) next ctx
            }
        
        

    let checkRedirect : HttpHandler =
        fun next ctx -> 
            if 
                [HttpMethods.Put ; HttpMethods.Patch; HttpMethods.Delete] |> List.contains ctx.Request.Method && 
                [ 301; 302] |> List.contains ctx.Response.StatusCode 
            then
                ctx.SetStatusCode 303
            next ctx

    let forceRefresh : HttpHandler =
        fun next ctx -> 
            ctx.SetHttpHeader("X-Inertia", "true")
            ctx.SetHttpHeader("X-Inertia-Location",ctx.Request.GetEncodedUrl())
            ctx.SetContentType("text/html")
            next ctx

    let checkInertiaRequestAndVersion (version:string) : HttpHandler =
        fun next ctx ->
            match ctx.Request.Headers.Inertia with
            | Some "true" when ctx.Request.Method = HttpMethods.Get ->
                // check asset version in context
                match ctx.Request.Headers.InertiaVersion with
                | Some a when a <> version ->
                    forceRefresh next ctx
                | _ -> next ctx
            | Some "true" ->
                checkRedirect next ctx
            | _ -> 
                next ctx

    let setResponse (withTemplate: string -> XmlNode) (page:Page) : HttpHandler =
        fun next ctx ->
            match ctx.Request.Headers.Inertia with
            | Some ("true") ->
                ctx.SetHttpHeader("Vary","Accept")
                ctx.SetHttpHeader("X-Inertia","true")
                ( page |> json) next ctx
            | None | Some _ ->
                (page.toJson() |> withTemplate |> htmlView) next ctx


[<AutoOpen>]
module Core =
    open Handlers

    
    let shareData (props:HttpContext -> Map<string,obj>) : HttpHandler =
        fun next ctx ->
            ctx.Items["InertiaSharedData"] <- (props ctx)
            next ctx

    let renderInertia componentName (props:Map<string,obj>) withTemplate assetsVersion url : HttpHandler =
        fun next ctx ->
            (checkInertiaRequestAndVersion assetsVersion
                >=> setCsrfCookie
                >=> generatePage (setResponse withTemplate) ctx componentName props url assetsVersion)
                next ctx