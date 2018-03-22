#r "System.Core.dll"
#r "System.Xml.Linq.dll"
open System.Xml
open System.Xml.Linq
open System.Text.RegularExpressions
open System.IO

let xmlPath = @"D:\Projects\NLP\cawiktionary\cawiktionary-20180301-pages-articles.xml"
let resultPath = @"D:\Projects\NLP\cawiktionary\result.txt"

let regSquareBrackets = Regex (@"(?<=\[\[).+?(?=\]\])", RegexOptions.Compiled)
let regFigureBrackets = Regex (@"(?<={{).+?(?=}})", RegexOptions.Compiled)

let getChild (childName:string) (el : XElement) =
    el.Element(XName.Get(childName,el.Name.NamespaceName))

let getValue (el : XElement) = el.Value

let findSynBlock (text:string) =
    let synLiteral = "{{-sin-}}"
    match text.IndexOf(synLiteral) with
    | -1 -> None
    | i -> 
        let left = i + synLiteral.Length
        let synBlock = 
            match text.IndexOf("{{-", left),text.IndexOf("===", left) with
            | -1,-1 -> text.Substring(left)
            | -1,right | right,-1 -> text.Substring(left, right - left)
            | ind1, ind2 -> text.Substring(left, (min ind1 ind2) - left)
        Some synBlock

let selectSyns (word:string) (text:string) = 
    let square = [for m in regSquareBrackets.Matches(text) -> m.Value] |> List.distinct
    let figure = [for m in regFigureBrackets.Matches(text) -> 
                    match m.Value.LastIndexOf('|') with
                    | -1 -> m.Value
                    | x -> m.Value.Substring(x + 1)]
    let synonyms = square @ figure |> List.distinct |> String.concat ", "
    sprintf "%s : %s" word synonyms
 
let parsePage (reader : XmlReader) : string option = 
    let el =  XNode.ReadFrom(reader) :?> XElement
    let title= getChild "title" el |> getValue
    if title.Contains(":")
    then None
    else el 
        |> getChild "revision"
        |> getChild "text"
        |> getValue
        |> findSynBlock
        |> (Option.map (selectSyns title))

let readXml (xmlUri:string) (outputPath:string) =
    let rec read (reader : XmlReader)= 
        seq {
            if reader.Read() then
                match reader.NodeType with
                | XmlNodeType.Element when reader.Name = "page" ->               
                    yield parsePage reader
                    yield! read reader
                | _ ->
                    yield! read reader                
            else
                ()
        }    
    use reader = XmlReader.Create(xmlUri)
    use file = new StreamWriter(outputPath)
    reader.ReadToDescendant("page") |> ignore
    reader
    |> read
    |> Seq.choose id
    |> Seq.iter (fun s -> file.WriteLine(s))
    

readXml xmlPath resultPath