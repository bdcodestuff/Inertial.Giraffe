module Reflection

open System
open FSharp.Linq.RuntimeHelpers
open Inertial.Giraffe
open Inertial.Giraffe.Types
open Inertial.Lib
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
                    
/// We assume that a Props value is a union case wrapping a single record value
/// This function takes the optional Props union case and returns a tuple of the inner union case boxed as obj and the type of the outer union
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
    | None -> failwith "No props found for evaluation"
    
// active recognizer for union cases by name    
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

let recordValueChoice2Evaluator (choiceOption:obj) (filter) (memberName) (asyncDataName: AsyncDataName) =
    async {
        match choiceOption with
        | UC <@ Choice1Of2 @> [v] when filter |> Array.contains memberName || filter |> Array.contains "*" ->
            let fsharpFuncArgs = choiceOption.GetType().GetGenericArguments()
            let asyncOfB = fsharpFuncArgs.[0]
            let genArgAsyncOfB = asyncOfB.GetGenericArguments()                   
            // B
            let typeBFromAsyncOfB = genArgAsyncOfB.[0]
            
            
            let asyncBoxer = typedefof<AsyncBoxer<_>>.MakeGenericType(typeBFromAsyncOfB)
                            |> Activator.CreateInstance 
                            :?> IAsyncBoxer
                            
            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                            |> Activator.CreateInstance 
                            :?> IChoice2Boxer
                                                                    
            let! asyncResult = asyncBoxer.BoxAsyncResult v
            let choiceResult = Choice2Of2 asyncResult
            let result = choice2Boxer.Reboxer choiceResult // Choice<Async<Option<'T>,Option<'T>>
            let resultBox = choice2Boxer.BoxChoice2Result result
            let resultAsyncDataBox = choice2Boxer.ReboxToAsyncData resultBox asyncDataName          
            
            return resultAsyncDataBox
        | UC <@ Choice1Of2 @> [v] ->
            let fsharpFuncArgs = choiceOption.GetType().GetGenericArguments()
                            
            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                            |> Activator.CreateInstance 
                            :?> IChoice2Boxer
            let choiceResult = Choice1Of2 v
            let result = choice2Boxer.Reboxer choiceResult // Choice<Async<Option<'T>,Option<'T>>
            let resultBox = choice2Boxer.BoxChoice2Result result
            let resultAsyncDataBox = choice2Boxer.ReboxToAsyncData resultBox asyncDataName
            return resultAsyncDataBox
        | _ -> return choiceOption
    }
let recordValueEvaluator
    (memberName: string) 
    (memberVal: obj) 
    (filter: string array)
    (prefix: string list) =
        async {
            let memberName =
                match prefix with
                | [] -> memberName
                | _::xs -> 
                    xs |> List.reduce (fun x y -> $"{x}.{y}") |> fun p -> $"{p}.{memberName}"
            match memberVal with
            | UC <@ Choice2List @> [choiceOption] ->
                return! recordValueChoice2Evaluator choiceOption filter memberName AsyncDataName.Choice2List
            | UC <@ Choice2Option @> [choiceOption] ->
                return! recordValueChoice2Evaluator choiceOption filter memberName AsyncDataName.Choice2Option
            | UC <@ Choice2ResultList @> [choiceOption] ->
                return! recordValueChoice2Evaluator choiceOption filter memberName AsyncDataName.Choice2ResultList
            | UC <@ Choice2OptionListDecoder @> [choiceOption] ->
                return! recordValueChoice2Evaluator choiceOption filter memberName AsyncDataName.Choice2OptionList
            | _ ->
                return memberVal
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
                            let! r = recordValueEvaluator m o [|"*"|] []
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
                                let! r = recordValueEvaluator m o f []
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

let recordEvaluator3 (r: obj) filter isPartial isFull =
    let rec inner (inboundExpr:Expr) filter isPartial isFull prefix : Async<Expr> =
        async {
            let typ = r.GetType()
            let objName = typ.Name
            
            
            
            return Expr.Value("string")
        }
    inner (Expr.Value(r)) filter isPartial isFull

let recordEvaluator2 (p: obj) filter isPartial isFull =

    let rec inner (r: obj) filter isPartial isFull prefix =
        async {
            let typ = r.GetType()
            let name = typ.Name
      
            
            if (FSharpType.IsRecord typ) then
                
                let recordTypes =
                    FSharpType.GetRecordFields(typ)
                    |> Array.map (_.PropertyType)
                let recordMemberNames =
                    FSharpType.GetRecordFields(typ)
                    |> Array.map (_.Name)
                
                if isFull then
                    let! evaluatedRecord =
                        FSharpValue.GetRecordFields(r)
                        |> Array.zip recordMemberNames
                        |> Array.map (fun (m,o) ->
                            async {
                                let! nested = inner o [|"*"|] true false (name::prefix)
                                let evaluated = LeafExpressionConverter.EvaluateQuotation nested
                                let! r = recordValueEvaluator m evaluated [|"*"|] prefix
                                return r
                            } )
                        |> Async.Parallel
                    let merged =
                        evaluatedRecord
                        |> Array.zip recordTypes
                        |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                    
                    let mergedRecord = Expr.NewRecord( typ, merged )
                    
                    return mergedRecord
                
                else
                    match filter with
                    | f when isPartial ->
                        let! evaluatedRecord =
                            FSharpValue.GetRecordFields(r)
                            |> Array.zip recordMemberNames
                            |> Array.map (fun (m,o) ->
                                async {
                                    let! nested = inner o f true false (name::prefix)
                                    let evaluated = LeafExpressionConverter.EvaluateQuotation nested
                                    let! r = recordValueEvaluator m evaluated f prefix
                                    return r
                                } )
                            |> Async.Parallel
                            
                        let merged =
                            evaluatedRecord
                            |> Array.zip recordTypes
                            |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                        
                        let mergedRecord = Expr.NewRecord(typ, merged )
                    
                        return mergedRecord

                    | _ ->
                        let record =
                            FSharpValue.GetRecordFields(r)
                            |> Array.zip recordTypes
                            |> Array.map (fun (t,o) -> Expr.Value(o,t)) |> List.ofArray
                        
                        let mergedRecord = Expr.NewRecord( typ, record )
                    
                        return mergedRecord
            // else if (FSharpType.IsUnion typ) then
            //     let ucValues = FSharpValue.GetUnionFields (r, typ)
            //     let outInfo, outObjs = ucValues
            //     let outerName = outInfo.Name
            //     let outObj = outObjs |> Seq.head // singe case union type only for now
            //     let! nested = inner outObj filter true false (outerName::prefix)
            //     let evaluated = nested |> LeafExpressionConverter.EvaluateQuotation
            //     let! innerEvaluated = inner evaluated filter isPartial isFull prefix
            //     return Expr.NewUnionCase(outInfo, [ innerEvaluated ])
            else
                //let! r = recordValueEvaluator name r filter prefix
                return Expr.Value(r,typ)
        }
        
    inner p filter isPartial isFull

   
let evaluated propsToEval filter isPartial isFull =
    async {
        let eval q = LeafExpressionConverter.EvaluateQuotation q
        let innerProps, outerPropsUCType = innerRecord propsToEval
        let! evaluatedInnerProps = recordEvaluator innerProps filter isPartial isFull
                
        // debug
        // evaluatedInnerProps |> List.iter (fun x -> printfn $"{x.Type.Name}")
        let newRcdExpr = Expr.NewRecord( innerProps.GetType(), evaluatedInnerProps )
        //let newRcdExpr = evaluatedInnerProps    
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