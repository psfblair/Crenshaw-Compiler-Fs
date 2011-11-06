﻿#light

module Crenshaw.Compiler

// Lookahead character
let mutable look = ' '

// Read New Character From Input Stream
let getChar() = look <- char (System.Console.Read())

// Report an Error
let error s = 
   System.Console.Beep()
   eprintfn "\n\nError: %s" s

// Report Error and Halt
let abort s =
   error s
   System.Environment.Exit 1

// Report What Was Expected
let expected s = abort(sprintf "%s Expected" s)

// matches a Specific Input Character
let matches x = if look = x then getChar() else expected (sprintf "'%c'" x)

// Recognize an Alpha Character
let isAlpha c = Array.exists (fun elem -> (System.Char.ToUpper c).Equals elem) [|'A'..'Z'|]

// Recognize a Decimal Digit
let isDigit c = Array.exists (fun elem -> c.Equals elem) [|'0'..'9'|]

// Get an Identifier
let getName() = 
   if not (isAlpha look) then expected "Name"
   let returnVal = System.Char.ToUpper  look
   getChar()
   returnVal

// Get a Number
let getNum() = 
   if not (isDigit look) then expected "Integer"
   let returnVal = look
   getChar()
   returnVal

// Output a String with Tab
let emit s = printf "\t%s" s

// Output a String with Tab and CRLF
let emitLn s = 
   emit s
   printfn("")

// Get a single number
let term() = getNum() |> sprintf "MOVE #%c,D0" |> emitLn

let add() =
   matches '+'
   term()
   emitLn "ADD (SP)+,D0"

let subtract() =
   matches '-'
   term()
   emitLn "SUB (SP)+,D0"
   emitLn "NEG D0"

let expression() = 
   term()
   while look.Equals '+' || look.Equals '-' do
      emitLn "MOVE D0,-(SP)"
      if look.Equals('+') then
         add()
      elif look.Equals('-') then
         subtract()
      else
         expected "Addop" 

// Initialize - main program
let init() = getChar()

[<EntryPoint>]
let Main args = 
   init()
   expression()
   0
