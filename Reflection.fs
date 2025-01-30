module Reflection

open System
open FSharp.Linq.RuntimeHelpers
open Inertial.Giraffe
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Types
                    
// Reflection magic here to extract inner page records and invoke async methods by string-based name
let innerRecord (props: 'Props option) =
    match props with
    | Some p ->
        let outerPropsType = p.GetType()
        if (FSharpType.IsUnion outerPropsType) then
            let out =
                FSharpValue.GetUnionFields (p, outerPropsType)
                |> snd
                |> Seq.head, outerPropsType
            out
        else failwith "Provided props are not in required union type format"
    | None -> failwith "No props found for async evaluation"
    
    
let (|UC|_|) e o =
      match e with
      | Lambdas(_,NewUnionCase(uc,_)) | NewUnionCase(uc,[]) ->
          if (box o = null) then
            // Need special case logic in case null is a valid value (e.g. Option.None)
            let attrs = uc.DeclaringType.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, false)
            if attrs.Length = 1
               && (attrs.[0] :?> CompilationRepresentationAttribute).Flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0
               && uc.GetFields().Length = 0
            then Some []
            else None
          else 
            let t = o.GetType()
            if FSharpType.IsUnion t then
              let uc2, fields = FSharpValue.GetUnionFields(o,t)
              let getGenType (t:System.Type) = if t.IsGenericType then t.GetGenericTypeDefinition() else t
              if uc2.Tag = uc.Tag && getGenType (uc2.DeclaringType) = getGenType (uc.DeclaringType) then
                Some(fields |> List.ofArray)
              else None
            else None
      | _ -> failwith "The UC pattern can only be used against simple union cases"
      
let recordValueEvaluator (memberName: string) (r: obj) (filter: string array) =
    async {
        match r with
        | UC <@ Choice1Of2 @> [v] when filter |> Array.contains memberName || filter |> Array.contains "*" ->
            // A -> Async<B>
            let fsharpFuncArgs = r.GetType().GetGenericArguments()
            let asyncOfB = fsharpFuncArgs.[0]
                                    
            // B
            let typeBFromAsyncOfB = asyncOfB.GetGenericArguments().[0]
            
            
            let asyncBoxer = typedefof<AsyncBoxer<_>>.MakeGenericType(typeBFromAsyncOfB)
                             |> Activator.CreateInstance 
                             :?> IAsyncBoxer
                             
            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                             |> Activator.CreateInstance 
                             :?> IChoice2Boxer
                                                                     
            let! asyncResult = asyncBoxer.BoxAsyncResult v
            let choiceResult = Choice2Of2 asyncResult
            let result = choice2Boxer.Reboxer choiceResult
            
            return result

        | _ -> return r
    }
    
let recordEvaluator (r: obj) filter isPartial isFull =
    async {
        let typ = r.GetType()
        let recordTypes =
            FSharpType.GetRecordFields(typ)
            |> Array.map (_.PropertyType)
        let recordMemberNames =
            FSharpType.GetRecordFields(typ)
            |> Array.map (_.Name)
        if (FSharpType.IsRecord typ) then
            if isFull then
                let! evaluatedRecord =
                    FSharpValue.GetRecordFields(r)
                    |> Array.zip recordMemberNames
                    |> Array.map (fun (m,o) ->
                        async {
                            let! r = recordValueEvaluator m o [|"*"|]
                            return r
                        } )
                    |> Async.Parallel
                let merged =
                    evaluatedRecord
                    |> Array.zip recordTypes
                    |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                return merged
            else
                match filter with
                | f when isPartial ->
                    let! evaluatedRecord =
                        FSharpValue.GetRecordFields(r)
                        |> Array.zip recordMemberNames
                        |> Array.map (fun (m,o) ->
                            async {
                                let! r = recordValueEvaluator m o f
                                return r
                            } )
                        |> Async.Parallel
                    let merged =
                        evaluatedRecord
                        |> Array.zip recordTypes
                        |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                    return merged

                | _ ->
                    let record =
                        FSharpValue.GetRecordFields(r)
                        |> Array.zip recordTypes
                        |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                    return record
        else
            return failwith "Provided props do not have required format of union type of records"
    }
    
let evaluated propsToEval filter isPartial isFull =
    async {
        let eval q = LeafExpressionConverter.EvaluateQuotation q
        let innerProps, outerPropsUCType = innerRecord propsToEval
        let! evaluatedInnerProps = (recordEvaluator innerProps filter isPartial isFull)
        // debug
        // evaluatedInnerProps |> List.iter (fun x -> printfn $"{x.Type.Name}")
        let newRcdExpr = Expr.NewRecord( innerProps.GetType(), evaluatedInnerProps )
            
        let expr =
          Expr.NewUnionCase(
            FSharpType.GetUnionCases(outerPropsUCType) |>
                function
                | [||] -> failwith "Provided props input does not have required format of a union type of records"
                | [|a|] -> a, [ newRcdExpr ] // single case union type (ie Props only has one case)
                | b -> b
                       |> Array.pick (
                                function
                                | x when x.Name = outerPropsUCType.Name -> Some x
                                | _ -> None),  [ newRcdExpr ]
                )

        return expr |> eval
    }
    
let resolver (p: 'Props) =
    let outerType = p.GetType()
    let unionCases = FSharpType.GetUnionCases(typeof<'Props>)
    
    let matchType =
        match unionCases with
        | [||] -> failwith "Props must be a union case"
        | [|a|] -> a // single case union case
        | uc ->
            uc |> Array.pick (
                function
                | x when x.Name = outerType.Name -> Some x
                | _ -> None)
    matchType.Name