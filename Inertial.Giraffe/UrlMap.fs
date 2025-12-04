namespace Inertial.Giraffe

open Giraffe
open System
open Giraffe.FormatExpressions
open Giraffe.EndpointRouting

module UrlMap =
    let paramMatchOptions = { IgnoreCase = true; MatchMode = MatchMode.Exact }

    type RouteHandler =
        | BasicRoute of HttpHandler
        | StringIntParamRoute of (string * int -> HttpHandler)
        | StringParamRoute of (string -> HttpHandler)
        | IntParamRoute of (int -> HttpHandler)
        member x.paramsFromPath (template:string) (path:string) =
            match x with 
            | BasicRoute _ -> []
            | StringParamRoute _ -> tryMatchInput (PrintfFormat<string->obj,obj,obj,obj,string>(template)) paramMatchOptions path |> Option.map (fun s -> s |> List.singleton ) |> function Some l -> l | None -> []
            | StringIntParamRoute _ -> tryMatchInput (PrintfFormat<string->obj,obj,obj,obj,string*int>(template)) paramMatchOptions path |> Option.map (fun (s,i) -> [ s; string i ] ) |> function Some l -> l | None -> []
            | IntParamRoute _ -> tryMatchInput (PrintfFormat<string->obj,obj,obj,obj,int>(template)) paramMatchOptions path |> Option.map (fun s -> string s |> List.singleton ) |> function Some l -> l | None -> []
        member x.endpointFromInterpolatedPath (template:string) =
            match x with
            | BasicRoute h -> route template h
            | StringParamRoute h -> routef (PrintfFormat<string->obj,obj,obj,obj,string>(template)) h
            | StringIntParamRoute h -> routef (PrintfFormat<string->obj,obj,obj,obj,string*int>(template)) h
            | IntParamRoute h -> routef (PrintfFormat<string->obj,obj,obj,obj,int>(template)) h
        member x.makePathFromInterpolatedPathAndParams (template:string) (parameters:string list) =
            match x, parameters with
            | BasicRoute _, [] -> template
            | StringParamRoute _, [s] -> template.Replace("%s",s)
            | StringIntParamRoute _, [s;i] -> template.Replace("%s",s).Replace("%i",i)
            | IntParamRoute _, [i] -> template.Replace("%i",i)
            | _ -> failwith "unable to construct url from provided params"
        member x.makeHandler (parameters:string list) =
            match x, parameters with
            | BasicRoute h, [] -> h
            | StringParamRoute h, [s] -> 
                h s 
            | StringIntParamRoute h, [s;i] -> 
                h (s,(int i))
            | IntParamRoute h, [i] -> 
                h (int i)
            | _ -> failwith "unable to retrieve handler from provided params"


    [<CustomEquality; CustomComparison>]
    type RouteData =
        {
            name : string
            method : string
            interpolatedPath : string
            handler : RouteHandler
        }

        member x.GetParametersFromPath (path:string) =
            x.handler.paramsFromPath x.interpolatedPath path
        member x.GetHandlerUsingParams (parameters:string list) =
            x.handler.makeHandler parameters
        member x.GetHandlerForSpecifiedPath (path:string) =
            let parameters = x.GetParametersFromPath path
            x.GetHandlerUsingParams parameters

        interface IEquatable<RouteData> with
            member this.Equals other = 
                other.name.Equals this.name && 
                other.method.Equals this.method && 
                other.interpolatedPath.Equals this.interpolatedPath

        override this.Equals other =
            match other with
            | :? RouteData as p -> (this :> IEquatable<_>).Equals p
            | _ -> false

        override this.GetHashCode () = this.name.GetHashCode()
        
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? RouteData as p -> (this :> IComparable<_>).CompareTo p
                | _ -> -1

        interface IComparable<RouteData> with
            member this.CompareTo other = 
                let urlCompare = other.name.CompareTo this.name
                match urlCompare with
                | 0 -> 
                    let methodCompare = other.method.CompareTo this.method
                    match methodCompare with
                    | 0 -> other.interpolatedPath.CompareTo this.interpolatedPath
                    | _ -> methodCompare
                | _ -> urlCompare

    let makeEndpoint name method path handler =
        {
            name = name
            method = method
            interpolatedPath = path
            handler = handler
        }

    let convertToUrlComponentMap (routeDataList: RouteData list) : (string * string) list =
        routeDataList |> List.map (fun ep -> ep.interpolatedPath, ep.name)
    let convertToEndpoints endpointMap =
        endpointMap
        |> List.fold
            (fun (gets,posts,puts,patches,deletes,ignores) data -> 
                let endpoint = data.handler.endpointFromInterpolatedPath data.interpolatedPath
                match data.method with
                | "GET" ->    
                    endpoint::gets,posts,puts,patches,deletes,ignores
                | "POST" ->
                    gets,endpoint::posts,puts,patches,deletes,ignores
                | "PUT" ->
                    gets,posts,endpoint::puts,patches,deletes,ignores
                | "PATCH" ->
                    gets,posts,puts,endpoint::patches,deletes,ignores
                | "DELETE" ->
                    gets,posts,puts,patches,endpoint::deletes,ignores
                | _ ->
                    gets,posts,puts,patches,deletes,endpoint::ignores) ([],[],[],[],[],[])
        |> fun (gets,posts,puts,patches,deletes,_) ->
            [
                GET gets
                POST posts
                PUT puts
                PATCH patches
                DELETE deletes
            ]

