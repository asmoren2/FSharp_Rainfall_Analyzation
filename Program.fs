//
//F# program to analyze rainfall data
//
//Author: Adolfo Moreno
//U. of Illinois, Chicago
//CS341, Fall 2015
//Homework 5
//

#light

//Reads file line by line and returns a list of string one per line
let ReadFile filename =
  [ for line in System.IO.File.ReadLines(filename) -> line]

//Given a line from a file. This parses the string into a list of double values
//starting with the year, error rate, and 12 rainfall vals one per month.
let ParseLine (line:string) = 
  let strings = line.Split('\t')
  let strlist = Array.toList(strings)
  let values = List.map (fun x-> System.Double.Parse(x,System.Globalization.NumberFormatInfo.InvariantInfo)) strlist
  values

let rec last n xs = 
  if List.length xs <= n then xs
  else last n xs.Tail

let byYear (values: double list) = 
  let L2 = last 12 (values)
  let average1 = List.average L2
  
  printfn "Year:%A Average:%A" (int values.Head) average1

let rec getAverageRecursive (L1: double list list) = 
  if L1 = [] then
    []
  else 
  L1.Head.Head::getAverageRecursive L1.Tail 

let averageHelp (L1: double list list) = 
  let L2 = getAverageRecursive L1
  let L3  = List.average L2
  L3

let rec takeHeadoff (L1: double list list) = 
  if L1 = [] then
    []
  else 
  let L5 = averageHelp L1
  L1.Head.Tail::takeHeadoff L1.Tail 

let rec averageFinal (L1: double list list) (Months: string list)= 
  if L1.Head = [] then 
   []
  else 
  let L3 = averageHelp L1
  printfn "%s : %A" Months.Head L3
  let L1 = takeHeadoff L1
  averageFinal L1 Months.Tail

 //Call maximum recursively to do max 
let maxRecurse (L1: double list) = 
  let L2 = List.max L1.Tail.Tail
  L2

//Get the minimum
let minRecurse (L1: double list) =
  let L2 = List.min L1.Tail.Tail
  L2

//Get the head of the list
let getHead(L1: double list) = 
   let L2 = L1.Head
   L2

let byYearRecursion L1 = 
  List.map (fun x -> byYear x) L1

let PrintOneYearOfData (values: double list) =
    printfn "%A: err=%A, %A" (int values.Head) values.Tail.Head values.Tail.Tail
   
[<EntryPoint>]
let main argv = 
    printfn "Rainfall Analysis Program"
    printfn ""

    //Read file as list of strings
    let file = ReadFile "rainfall-midway.txt"

    //Parse data into a list of lists, where each
    //sub-list is[ year err value value...]
    let rainfall = List.map ParseLine file
    let (Months: string list) = ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]

    //let L2 = last 12 rainfall.Head
    //let average1 = List.average L2

    byYearRecursion rainfall

    //Find max in list
    let max = List.map maxRecurse rainfall
    let maxRain = List.max max
    let maxIndex = List.findIndex (fun x -> x >= maxRain) max
    
    //Find min in list
    let min = List.map minRecurse rainfall
    let minRain = List.min min
    let minIndex = List.findIndex (fun x -> x <= minRain) min

    let Header = List.map getHead rainfall
    let Header2 = List.rev Header
    let indexer = List.nth Header minIndex
    let finalList = List.nth rainfall minIndex
    let findMonth = List.findIndex (fun x-> x = minRain) finalList
    let finalMonth = findMonth - 2
    let monthFin = List.nth Months finalMonth

    //Find max in list
    let maxHead = List.map getHead rainfall
    let maxHead2 = List.rev maxHead
    let maxIndexer = List.nth maxHead maxIndex
    let finalMaxList = List.nth rainfall maxIndex
    let findMaxMonth = List.findIndex (fun x -> x = maxRain) finalMaxList
    let finalMaxMonth = findMaxMonth - 2
    let MonthMaxFinal = List.nth Months finalMaxMonth
     
    let rain1 = takeHeadoff rainfall
    let rain1 = takeHeadoff rain1

    averageFinal rain1 Months

    printfn "Min Rainfall: Year: %A Month: %s Min: %A" (int indexer) monthFin minRain 
    printfn "Max Rainfall: Year: %A Month: %s Max: %A" (int maxIndexer) MonthMaxFinal maxRain
    //Deugging
    printfn ""

    //Done
    printfn ""
    printfn"** Done **"
    printfn ""
    printfn ""

    0 // return an integer exit code