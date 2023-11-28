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

module Dynamic =
    open System
    open FSharp.Reflection

    type InvokeResult = 
        | Success of obj
        | ObjectWasNotAFunction of Type

    let dynamicFunction (fn:obj) (args:obj seq) =
        let rec dynamicFunctionInternal (next:obj) (args:obj list) : InvokeResult =
            match args.IsEmpty with
            | false ->
                let fType = next.GetType()
                if FSharpType.IsFunction fType then
                    let (head, tail) = (args.Head, args.Tail)
                    let methodInfo = 
                        fType.GetMethods()
                        |> Seq.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                        |> Seq.head
                    let partalResult = methodInfo.Invoke(next, [| head |])
                    dynamicFunctionInternal partalResult tail
                else ObjectWasNotAFunction fType
            | true ->
                Success(next)
        dynamicFunctionInternal fn (args |> List.ofSeq )

module Handlers =

    open FSharpx.Collections
    open FSharp.Reflection
    
    let handleProps (ctx:HttpContext) componentName (props:Map<string,obj>) =
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
        
        let finalProps =
            // props with type fun () -> obj always included on first visit, optionally on partial reloads, only evaluated when needed
            if isPartialReq then
                functions
                |> Map.map (fun _ y ->
                    let ty = y.GetType()
                    let tyFrom, tyTo = FSharpType.GetFunctionElements(ty)
                    match y with 
                    | :? (unit -> obj) as f -> f ()
                    | :? (unit -> Task<obj>) as f -> f () |> Async.AwaitTask |> Async.RunSynchronously
                    | :? (unit -> Async<obj>) as f -> f () |> Async.RunSynchronously
                    | b -> failwith $"unable to handle func prop with type: {b.GetType()}" )
            else 
                nonFunctions
        finalProps

    let setCsrfCookie : HttpHandler =
        fun next ctx -> 
            let tokenSet = ctx.GetService<IAntiforgery>().GetTokens(ctx)
            let options = new CookieOptions()
            options.SameSite <- SameSiteMode.Strict
            ctx.Response.Cookies.Append("XSRF-TOKEN",tokenSet.CookieToken,options)
            next ctx

    let generatePage (nextHandler: Page -> HttpHandler) ctx componentName (props:Map<string,obj>) (url:string option) version : HttpHandler =
        {
            ``component`` = componentName
            props = handleProps ctx componentName props
            version = version
            url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
        }
        |> nextHandler

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