//Ankita Tank
//U. OF ILLINOIS, CHICAGO
//CS 341, FALL 2016
//HW #04
//
#light
// Prompt user for a folder name, and then return a list of all the 
// files in that folder.  Returns empty list if folder doesn't exist
// or cannot be found.
//
let GetFilenames() = 
  printf "Please enter folder name> "
  let folder = System.Console.ReadLine()
  //printfn "%A" System.AppDomain.CurrentDomain.BaseDirectory
  // ignore(printfn ")
  let dir = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, folder)
  if not (System.IO.Directory.Exists dir) then
    []
  //printfn "no"
  else
    dir
    |> System.IO.Directory.GetFiles
    |> Array.toList
//
// Parse one line of CSV data from a Divvy data file.  The format of each data line
// is as follows:
//
// trip_id,bikeid,tripduration,from_station_id,to_station_id,gender,birthyear
//  8677704,3808,311,283,47,Male,1984
//  ...
//  8677694,170,394,44,44,?,0
//  ...
//  8677645,2452,228,129,274,Female,1987
//  ... 
// 
// Right now the function returns a list of  two integer elements: [tripID; bikeID].
// You will need to change this.
//
let private ParseLine (line:string) = 
  let elements = line.Split(',')
  let tripID = System.Convert.ToInt32(elements.[0])
  let bikeID = System.Convert.ToInt32(elements.[1])
  let duration = System.Convert.ToInt32(elements.[2])
  let fromStationID = System.Convert.ToInt32(elements.[3])
  let toStationID = System.Convert.ToInt32(elements.[4])
  let gender = elements.[5]
  let birthYear = System.Convert.ToInt32(elements.[6])
  //let age = System.Convert.ToInt32(elements.[7])
  //
  // how do you want to store the data?  you can return the values as 
  // a small list of integers [tripID;bikeID;...], or you can return
  // the values as a tuple (tripID, bikeID, ...).  There's really no 
  // difference other than how you process, and that tuples allow you 
  // to mix types, while a list requires that all values be the same
  // type.  This implies you would need to convert the gender --- "Male",
  // "Female", or "?" to an integer code (which might be a smart move
  // anyway.
  //
  (
    (tripID, bikeID, duration,fromStationID,toStationID, gender,birthYear )
  )
 
// Parses 1 file of Divvy data, where the format of each line is 
// discussed above; returns back a list of elements where the format
// of each element is discussed above.
//
// NOTE: the "|>" means pipe the data from one function to
// the next.  The code below is equivalent to letting a var
// hold the value and then using that var in the next line.
//
let private ParseDivvyFile filename = 
  System.IO.File.ReadLines(filename)
  |> Seq.skip 1  // skip header row:
  |> Seq.map ParseLine
  |> Seq.toList
let getMembers list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "?") && (birthYear <> 0) )
  ret 
let getNonMembers list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Female") && (gender <> "Male") )
  ret 
  
let getMemberss list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "?") && (birthYear <> 0) )
  let floatgetmem =float(List.length ret)
  floatgetmem 
let getMales list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Female") && (birthYear <> 0)&&(gender <> "?"))
  let floatgetmale =float(List.length ret)
  floatgetmale 
let getMalesInt list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Female") && (birthYear <> 0)&&(gender <> "?"))
  let intgetmale =List.length ret
  intgetmale
let getfemales list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Male") && (birthYear <> 0) && (gender <> "?"))
  let floatgetFemale =float(List.length ret)
  floatgetFemale
let age list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (birthYear < 0 ))
  ret
let getMalesAgeHelper list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Female") && (birthYear <> 0) && (gender <> "?"))
  ret
let getFeMalesAgeHelper list =
  let ret = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (gender <> "Male") && (birthYear <> 0) && (gender <> "?"))
  ret
let curYear =System.DateTime.Now.Year
let newage list =
  let g = list |> List.map( fun (_, _, _, _, _, _, birthYear) -> float((curYear - birthYear)))
  let gfloat = float( List.average g)
  gfloat
  
let newfemaleage list =
  let g = list |> List.map( fun (_, _, _, _, _, _, birthYear) -> float((curYear - birthYear)))
  let gfloat = float( List.average g)
  gfloat
  //***********************************************************************************************************************************************//
let getcount30 list =
  let count = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (duration <= (60*30)) )
  let ccount = List.length count
  ccount 
let getcount60 list =
  let count60 = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (duration <= (60*60)) && (duration > (60*30))  )
  let ccount60 = List.length count60
  ccount60 
let getcount120 list =
  let count120 = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (duration <= (60*120)) && (duration > (60*60))  )
  let ccount120 = List.length count120
  ccount120 
let getcount121 list =
  let count121 = list |> List.filter(fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (duration > (60*120))  )
  let ccount121 = List.length count121
  ccount121
[<EntryPoint>]
let main argv = 
  printfn ""
  printfn "** Divvy Ride Analysis **"
  printfn ""
  //
  let files = GetFilenames()
  //
  // input the data into a LIST OF LISTS, one per file.  The format
  // of each sub-list is determined by the "ParseDivvyFile" function:
  //
  let data = List.map ParseDivvyFile files
 
  printfn ""
  printfn "** # of rides:  %A" (List.map List.length data)
  let memberList = List.map getMembers data
  printfn "** Members:     %A" (List.map List.length memberList)
  let NonmemberList= List.map getNonMembers data
  printfn "** Non-members: %A" (List.map List.length NonmemberList)
  printfn "\n"
  let male= List.map getMales data
  let female= List.map getfemales data
  let total = List.map getMemberss data
  let percentmen = List .map2(fun x y -> (x/y)*100.00) male total 
  printfn "** %% of men:    %A" percentmen
  let percentwomen = List .map2(fun x y -> (x/y)*100.00) female total 
  printfn "** %% of women:  %A" percentwomen
  //**************************************************************************//
  printfn ""
  printfn "** Average Age:"
  let males= List.map getMalesAgeHelper data
  let females= List.map getFeMalesAgeHelper data
  let conct = List.concat males
  let concat = List.concat females
  let newmalesage= newage conct
  printf "   Men: %A" newmalesage
  printfn ""
  let newfemaleage= newfemaleage concat
  printf "   Women: %A " newfemaleage
  printfn ""
  printfn ""
  printfn "** Ride Duration:s"
  let c30 = List.map getcount30 data
  let sumc30 =  List.sum c30
  
  let c60 = List.map getcount60 data
  let sumc60 =  List.sum c60
 
  let c120 = List.map getcount120 data
  let sumc120 =  List.sum c120
 
  let c121 = List.map getcount121 data
  let sumc121 =  List.sum c121
  
  let total = sumc30+sumc60+sumc120+sumc121
  let p30 = (float (sumc30)/float(total) *100.00)
  let p60 = (float (sumc60)/float(total) *100.00)
  let p120 = (float (sumc120)/float(total) *100.00)
  let p121 = (float (sumc121)/float(total) *100.00)
  printfn "   1..30    mins: %A (%A%%)" sumc30 p30
  printfn "   31..60   mins: %A (%A%%)" sumc60 p60
  printfn "   61..120  mins: %A (%A%%)" sumc120 p120
  printfn "   121+     mins: %A (%A%%)" sumc121 p121
  printfn ""
  printfn "** Done **"
  printfn ""
  printfn ""
  0