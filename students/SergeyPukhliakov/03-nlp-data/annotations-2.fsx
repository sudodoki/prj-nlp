#r "System.Core.dll"
#r "System.Xml.Linq.dll"
open System.Xml
open System.Xml.Linq

type Pos = {
    par : int*int
    offset : int * int
}

type Mistake = {
    pos : Pos
    mistakeType : string  }

type Annotation = {
    documentId : int
    teacherId : int
    mistakes : Mistake list
}
let xmlUri1 = @"D:\Projects\NLP\conll14st-test-data\noalt\official-2014.0.sgml"
let xmlUri2 = @"D:\Projects\NLP\conll14st-test-data\noalt\official-2014.1.sgml"

let parseMistake (xEl: XElement) = {
    pos = {
            par = xEl.Attribute(XName.Get "start_par").Value |> int, xEl.Attribute(XName.Get "end_par").Value |> int
            offset = xEl.Attribute(XName.Get "start_off").Value |> int, xEl.Attribute(XName.Get "end_off").Value |> int }
    mistakeType = xEl.Element(XName.Get "TYPE").Value }

let parseDocAnnotation (reader : XmlReader) = 
    let docId = reader.GetAttribute("nid") |> int   
    reader.ReadToDescendant("ANNOTATION") |> ignore
    let el =  XNode.ReadFrom(reader) :?> XElement
    let teacherId = el.Attribute(XName.Get "teacher_id").Value |> int
    let mistakes = el.Elements() |> Seq.map (parseMistake) |> List.ofSeq
    {documentId = docId; teacherId = teacherId; mistakes = mistakes}


let readXml (xmlUri:string) : Annotation list  =
    let rec read (reader : XmlReader) = 
        seq {
            if reader.Read() then
                match reader.NodeType with
                | XmlNodeType.Element when reader.Name = "DOC" ->               
                    yield parseDocAnnotation reader
                    yield! read reader 
                | _ ->
                    yield! read reader                
            else
                ()
        }    

    let settings = new XmlReaderSettings()
    settings.ConformanceLevel <- ConformanceLevel.Fragment
    use reader = XmlReader.Create(xmlUri, settings)
    reader |> read |> List.ofSeq
    
let ann1 = readXml xmlUri1
let ann2 =readXml xmlUri2

let matchDiffCount (matchType:bool) mistakes1 mistakes2 = 
    let rec check (mistakes1,mistakes2) (matched,diff) =     
        match mistakes1,mistakes2 with
        | [],[] -> matched,diff
        | [],_ -> (matched, diff + mistakes2.Length)
        | _,[] -> (matched, diff + mistakes1.Length)    
        | (m1::rest1),(m2::rest2) ->
            match m1,m2 with
            | m1,m2 when m1 = m2 -> check (rest1,rest2) (matched + 2, diff)
            | m1,m2 when m1.pos = m2.pos -> 
                if matchType then
                    check (rest1,rest2) (matched, diff + 2)
                else       
                    check (rest1,rest2) (matched + 2, diff)        
            | m1,m2 when m1.pos.par = m2.pos.par ->            
                match m1.pos.offset, m2.pos.offset with
                | (startOff1, _),(startOff2, _) when startOff1 = startOff2 -> 
                    check (rest1,rest2) (matched, diff + 2)
                | (startOff1, _),(startOff2, _) when startOff1 < startOff2 ->
                    check (rest1,mistakes2) (matched, diff + 1)
                | (startOff1, _),(startOff2, _) when startOff1 > startOff2 ->
                    check (mistakes1,rest2) (matched, diff + 1)
                | _,_ -> failwith ("unexpected behaviour")
            | _,_ ->
                match m1.pos.par, m2.pos.par with
                | (parStart1, _),(parStart2, _) when parStart1 < parStart2 ->
                    check (rest1,mistakes2) (matched, diff + 1)
                | (parStart1, _),(parStart2, _) when parStart1 > parStart2 ->
                    check (mistakes1,rest2) (matched, diff + 1)
                | _,_ -> failwith ("unexpected behaviour")
    check (mistakes1, mistakes2) (0,0)

let matchAnnotations matchTypes zippedAnnotations =
    zippedAnnotations |>
    List.fold (fun (matched, diff) (an1,an2) ->
        let m,d = matchDiffCount  matchTypes an1.mistakes an2.mistakes
        (m + matched, d + diff) ) (0,0)

let matchAnnotationsByType mistakeType zippedAnnotations =
    zippedAnnotations |>
    List.fold (fun (matched, diff) (an1,an2) ->
        let filtered1 = List.filter (fun m -> m.mistakeType = mistakeType) an1.mistakes
        let filtered2 = List.filter (fun m -> m.mistakeType = mistakeType) an2.mistakes
        let m,d = matchDiffCount  true filtered1 filtered2
        (m + matched, d + diff) ) (0,0)


let mistakeTypes =
    (ann1 @ ann2) 
    |> List.collect (fun an -> an.mistakes |> List.map (fun m -> m.mistakeType))
    |> List.distinct
let zippedAnnotations = List.zip ann1 ann2

let percent (matched,diff) = (float matched) / (float diff) * 100.0

printfn "Agreement with matching mistake type: %f%%" (matchAnnotations true zippedAnnotations |> percent)
//Agreement with matching mistake type: 39.639200%

printfn "Agreement ignore mistake type: %f%%" (matchAnnotations false zippedAnnotations |> percent)
//Agreement ignore mistake type: 68.272620%

for t in mistakeTypes do
    printfn "Agreement for mistake type %s: %f%%" t (matchAnnotationsByType t zippedAnnotations |> percent)
// Agreement for mistake type ArtOrDet: 69.432314%
// Agreement for mistake type Nn: 93.449782%
// Agreement for mistake type Trans: 19.310345%
// Agreement for mistake type Mec: 31.464738%
// Agreement for mistake type Prep: 64.657534%
// Agreement for mistake type Wci: 26.584235%
// Agreement for mistake type Wform: 60.714286%
// Agreement for mistake type Vform: 28.901734%
// Agreement for mistake type Vt: 69.461078%
// Agreement for mistake type Ssub: 11.428571%
// Agreement for mistake type Rloc-: 7.299270%
// Agreement for mistake type V0: 36.000000%
// Agreement for mistake type Npos: 54.545455%
// Agreement for mistake type Pref: 16.901408%
// Agreement for mistake type SVA: 78.620690%
// Agreement for mistake type Vm: 7.500000%
// Agreement for mistake type Sfrag: 0.000000%
// Agreement for mistake type Pform: 18.181818%
// Agreement for mistake type Wtone: 0.000000%
// Agreement for mistake type Um: 0.000000%
// Agreement for mistake type Others: 14.814815%
// Agreement for mistake type Spar: 0.000000%
// Agreement for mistake type WOadv: 18.181818%
// Agreement for mistake type WOinc: 8.571429%
// Agreement for mistake type Srun: 6.451613%
// Agreement for mistake type Smod: 0.000000%
// Agreement for mistake type Wa: 0.000000%
