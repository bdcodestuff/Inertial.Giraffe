module FsInertia

open System
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
        Component : string
        Props : Map<string,obj>
        Version : string
        Url : string
    }

    member x.toJson () =
        JsonSerializer.Serialize(x)

module Handlers =

    let handleProps (ctx:HttpContext) componentName (props:Map<string,obj>) =
        let isPartialReq, filter =
            match ctx.Request.Headers.InertiaPartialData, ctx.Request.Headers.InertiaPartialComponent with
            | Some partialData, Some comp when comp = componentName ->
                true,
                partialData.Split(',') 
                |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
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
                    Map.fold (fun acc key value -> Map.add key value acc) filteredProps sharedProps
                with exn -> failwith exn.Message
        // Lazy load if full load; eval on partial 
        if isPartialReq then
            props |> Map.map (fun _ y -> match y with :? Func<unit,obj> as f -> f.Invoke( () ) | b -> b  )
        else 
            props

    let setCsrfCookie : HttpHandler =
        fun next ctx -> 
            let tokenSet = ctx.GetService<IAntiforgery>().GetTokens(ctx)
            ctx.Response.Cookies.Append("XSRF-TOKEN",tokenSet.CookieToken)
            next ctx

    let generatePage (nextHandler: Page -> HttpHandler) ctx componentName (props:Map<string,obj>) (url:string option) version : HttpHandler =
        {
            Component = componentName
            Props = handleProps ctx componentName props
            Version = version
            Url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
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

    
    let shareData (props:Map<string,obj>) : HttpHandler =
        fun next ctx ->
            ctx.Items["InertiaSharedData"] <- props
            next ctx

    let renderInertia componentName (props:Map<string,obj>) withTemplate assetsVersion url : HttpHandler =
        fun next ctx ->
            (checkInertiaRequestAndVersion assetsVersion
                >=> setCsrfCookie
                >=> generatePage (setResponse withTemplate) ctx componentName props url assetsVersion)
                next ctx