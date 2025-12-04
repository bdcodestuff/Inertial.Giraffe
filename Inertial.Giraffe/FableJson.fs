module FableJson

open Fable.Remoting.Json
open Newtonsoft.Json
open System.Text
open System.IO

let fableConverter = FableJsonConverter() :> JsonConverter

//let settings = JsonSerializerSettings(DateParseHandling = DateParseHandling.None)
let Formatting = Microsoft.FSharpLu.Json.Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.formatting
let Settings = Microsoft.FSharpLu.Json.Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.settings

let fableSerializer =
    let serializer = JsonSerializer()
    serializer.Converters.Add fableConverter
    serializer

let jsonEncoding = UTF8Encoding false

let jsonSerialize (o: 'a) (stream: Stream) =
    use sw = new StreamWriter (stream, jsonEncoding, 1024, true)
    use writer = new JsonTextWriter (sw, CloseOutput = false)
    fableSerializer.Serialize (writer, o)
    