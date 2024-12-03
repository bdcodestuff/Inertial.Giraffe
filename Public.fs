namespace Inertial.Giraffe

open Microsoft.FSharp.Core
open Newtonsoft.Json
open Microsoft.AspNetCore.Http
open FableJson
open Types

[<AutoOpen>]
module Public =
    type RealTimePredicates =
    | ComponentIsOneOf of string array
    | ComponentIsAnyExcept of string array
    | UserIdIsOneOf of string array
        
    type PropsToEval =
    | EvalAllProps
    | OnlyEvalProps of string array
       
    type Predicates =
        {
            predicates : RealTimePredicates array
            propsToEval : PropsToEval
        }
    type InertialSSEEvent =
        {
            title : string
            connectionId: string option
            predicates : Predicates
            firedOn : System.DateTime
        }
        static member empty () =
            { title = ""; connectionId = None; predicates = { predicates = [||]; propsToEval = EvalAllProps  } ; firedOn = System.DateTime.UtcNow }.toJson()
        
        /// Create a new SSE event that is sent to client as json for decoding
        static member create (ctx:HttpContext) (title:string) (propsToEvaluate: PropsToEval) (ifPredicatesMatch : RealTimePredicates list)  =
            {
                title = title
                connectionId = ctx.Request.InertiaId
                predicates = { predicates = (Array.ofList ifPredicatesMatch) ; propsToEval = propsToEvaluate }
                firedOn = System.DateTime.UtcNow
            }
                .toJson()
            
        member x.toJson() = JsonConvert.SerializeObject(x,converters=[|fableConverter|])
        
    type Page<'Props,'Shared> =
        {
            ``component`` : string
            version : string
            connectionId: string
            url : string
            title : string
            props : 'Props option
            refreshOnBack : bool
            reloadOnMount : {| shouldReload : bool; propsToEval: PropsToEval option |}
            realTime : bool
            shared : 'Shared
        }    
        member x.toJson() =
            JsonConvert.SerializeObject(x,converters=[|fableConverter|])

