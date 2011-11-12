#light

module Crenshaw.Compiler

// Lookahead character
let mutable look = ' '

// Read New Character From Input Stream
let getChar() = look <- char (System.Console.ReadKey().KeyChar)

// Report an Error
let error s = 
   System.Console.Beep()
   eprintfn "\n\nError: %s" s

// Report Error and Halt
let abort s =
   error s
   System.Environment.Exit 1

// Output a String with Tab
let emit s = printf "\t%s" s

// Output a String with Tab and CRLF
let emitLn s = 
   emit s
   printfn("")

// Report What Was Expected
let expected s = abort(sprintf "%s Expected" s)

// Recognize an Alpha Character
let isAlpha c = Array.exists (fun elem -> (System.Char.ToUpper c).Equals elem) [|'A'..'Z'|]

// Recognize a Decimal Digit
let isDigit c = Array.exists (fun elem -> c.Equals elem) [|'0'..'9'|]

// Recognize an Alphanumeric
let isAlNum c = isAlpha c || isDigit c

// Recognize White Space
let isWhite c = Array.exists (fun elem -> c.Equals elem) [|' '; '\t'|]

// Skip Over Leading White Space
let skipWhite() = 
   while isWhite look do
      getChar()
   done

// Match a Specific Input Character
let matchThenFetchNextChar x = 
   if look = x then 
      getChar()
      skipWhite()
   else expected (sprintf "'%c'" x)

// Get an Identifier
let getName() = 
   if not (isAlpha look) then expected "Name"
   let mutable token = ""
   while isAlNum look do
      token <- token + (System.Char.ToUpper look).ToString()
      getChar()
   done
   skipWhite()
   token

// Get a Number
let getNum() = 
   if not (isDigit look) then expected "Integer"
   let mutable value = ""
   while isDigit look do
      value <- value + (System.Char.ToUpper look).ToString()
      getChar()
   done
   skipWhite()
   value

// Parse and Translate an Identifier
let ident() =
   let name = getName()
   if look.Equals '(' then
      matchThenFetchNextChar '('
      matchThenFetchNextChar ')'
      sprintf "BSR %s" name |> emitLn 
   else
      sprintf "MOVE %s(PC),D0" name |> emitLn

// Horrible big circle of functions: let rec... and ... and... etc.
// Result of recursive descent.
// Parse and Translate a Math Factor
let rec factor() = 
   if look.Equals '(' then
      matchThenFetchNextChar '('
      expression()
      matchThenFetchNextChar ')'
   else if isAlpha look then
      ident()
   else
      getNum() |> sprintf "MOVE #%s,D0" |> emitLn

and multiply() =
   matchThenFetchNextChar '*'
   factor()
   emitLn "MULS (SP)+,D0"

and divide() =
   matchThenFetchNextChar '/'
   factor()
   emitLn "MOVE (SP)+,D1"
   emitLn "DIVS D1,D0"

//  Parse and Translate a Math Term
and term() = 
   factor()
   while  look.Equals '*' || look.Equals '/' do
      emitLn "MOVE D0,-(SP)"
      if look.Equals('*') then
         multiply()
      elif look.Equals('/') then
         divide()
      else
         expected "Mulop" 

// Recognize and Translate an Add
and add() =
   matchThenFetchNextChar '+'
   term()
   emitLn "ADD (SP)+,D0"

// Recognize and Translate a Subtract
and subtract() =
   matchThenFetchNextChar '-'
   term()
   emitLn "SUB (SP)+,D0"
   emitLn "NEG D0"

// Recognize an Addop
and isAddOp c =
   c.Equals '+' || c.Equals '-'

// Parse and Translate an Expression
and expression() = 
   if isAddOp look then
      emitLn "CLR D0"
   else
      term()
   while isAddOp look do
      emitLn "MOVE D0,-(SP)"
      if look.Equals('+') then
         add()
      elif look.Equals('-') then
         subtract()
      else
         expected "Addop" 

// Parse and Translate an Assignment Statement
let assignment() =
   let name = getName()
   matchThenFetchNextChar '='
   expression()
   sprintf "LEA %s(PC),A0" name |> emitLn
   emitLn "MOVE D0,(A0)"

// Initialize - main program
let init() = 
   getChar()
   skipWhite()

[<EntryPoint>]
let Main args = 
   init()
   assignment()
   if not (look.Equals '\n') then
      expected "Newline"
   0
