(*
   Copyright 2015 Philip Curzon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open NovelFS.NovelIO
open System.Net
open System.Text

[<EntryPoint>]
let main argv = 
//    let read3Tuple =
//        ioformatter{
//            let! a = BinaryIO.readFloat32()
//            let! b = BinaryIO.readFloat64()
//            let! c = BinaryIO.readInt16()
//            return (a, b, c)
//        }
    //let validPath = File.assumeValidFilename "test.txt"
    //let handle = IO. validPath
    //let test = BinaryIO.run read3Tuple handle
    //match test with
    //|IOSuccess res -> printfn "%A" res

    let fName = File.assumeValidFilename "test4.txt"

    let test = io {
        let! lines = File.readLines fName
        return! IO.mapM_ (Console.printfn "%s") (Seq.toList lines)
        }

    let testServ = io {
        let! serv = TCP.createServer (System.Net.IPAddress.Any) (7826)
        let! acceptSock = TCP.acceptConnection serv
        let! handle = TCP.socketToHandle acceptSock
        let! request = IO.takeWhileM (fun str -> str <> "" |> IO.return') (IO.hGetLine handle)
        do! IO.hPutStrLn handle ("HTTP/1.1 200 OK")
        do! IO.hPutStrLn handle ("Content-Type: text/html")
        do! IO.hPutStrLn handle ("Content-Length: 15")
        do! IO.hPutStrLn handle ("")
        do! IO.hPutStrLn handle ("<html></html>")
        }
        


    let test = IO.run testServ

    //let lines = File.assumeValidFilename "test3.txt" |> IO.readLines |> IO.run
    //match lines with
    //|IOSuccess a -> a |> Seq.iter (printfn "%s")
    //|IOError err -> printfn "%A" err


    //let testLines = System.IO.File.ReadLines "test4.txt"

    //testLines |> Seq.iter (printfn "%s")


    //testLines |> Seq.iter (printfn "%s")

    0

     