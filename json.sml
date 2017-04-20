(* An almost-RFC-compliant minimal JSON parser with no dependency on
   anything outside the Basis library.

   Notes:

   * Not suitable for large input files

   * We only support UTF-8 input, but we don't check that JSON strings
     contain valid UTF-8 and we don't expand \u escapes (our one
     non-compliance)

   * We convert all numbers to type "real". If that is a 64-bit IEEE
     float type then we're pretty standard for a JSON parser, but that
     isn't guaranteed in SML.

   A lot of this is based on the JSON parser in the Ponyo library by
   Phil Eaton.

   Reference: RFC 7159, The JavaScript Object Notation (JSON) Data
   Interchange Format.
*)

signature JSON_PARSER = sig

    datatype json = OBJECT of (string * json) list
                  | ARRAY of json list
                  | NUMBER of real
                  | STRING of string
                  | BOOL of bool
                  | NULL

    datatype 'a result = OK of 'a
                       | ERROR of string

    val parse : string -> json result

end

structure JsonParser :> JSON_PARSER = struct

    datatype json = OBJECT of (string * json) list
                  | ARRAY of json list
                  | NUMBER of real
                  | STRING of string
                  | BOOL of bool
                  | NULL

    datatype 'a result = OK of 'a
                       | ERROR of string

    structure T = struct
        datatype token = NUMBER of real
                       | STRING of string
                       | BOOL of bool
                       | NULL
                       | CURLY_L
                       | CURLY_R
                       | SQUARE_L
                       | SQUARE_R
                       | COLON
                       | COMMA

        fun toString t =
            case t of NUMBER r => Real.toString r
                    | STRING s => s
                    | BOOL b => Bool.toString b
                    | NULL => "null"
                    | CURLY_L => "{"
                    | CURLY_R => "}"
                    | SQUARE_L => "["
                    | SQUARE_R => "]"
                    | COLON => ":"
                    | COMMA => ","
    end

    fun error pos text = ERROR (text ^ " at position " ^ Int.toString (pos - 1))
    fun token_error pos expected =
        error pos ("Malformed token (expected \"" ^ expected ^ "\")")

    fun lexNull pos acc (#"u" :: #"l" :: #"l" :: xs) =
        lex (pos + 3) (T.NULL :: acc) xs
      | lexNull pos acc _ = token_error pos "null"

    and lexTrue pos acc (#"r" :: #"u" :: #"e" :: xs) =
        lex (pos + 3) (T.BOOL true :: acc) xs
      | lexTrue pos acc _ = token_error pos "true"

    and lexFalse pos acc (#"a" :: #"l" :: #"s" :: #"e" :: xs) =
        lex (pos + 4) (T.BOOL false :: acc) xs
      | lexFalse pos acc _ = token_error pos "false"

    and lexChar tok pos acc xs =
        lex pos (tok :: acc) xs
        
    and lexString pos acc cc =
        let fun lexString' pos text escaped [] =
                if escaped
                then error pos "End of input during escape sequence"
                else error pos "End of input during string"
              | lexString' pos text escaped (x :: xs) =
                if escaped
                then
                    let fun esc c = lexString' (pos + 1) (c :: text) false xs
                    in case x of
                           #"\"" => esc x
                         | #"\\" => esc x
                         | #"/"  => esc x
                         | #"b"  => esc #"\b"
                         | #"f"  => esc #"\f"
                         | #"n"  => esc #"\n"
                         | #"r"  => esc #"\r"
                         | #"t"  => esc #"\t"
                         | _     => error pos ("Invalid escape \\" ^
                                               Char.toString x)
                    end
                else case x of
                         #"\"" => OK (rev text, xs, pos + 1)
                       | #"\\" => lexString' (pos + 1) text true xs
                       | _     => lexString' (pos + 1) (x :: text) false xs
        in
            case lexString' pos [] false cc of
                OK (text, rest, newpos) =>
                lex newpos (T.STRING (implode text) :: acc) rest
              | ERROR e => ERROR e
        end
            
    and lexNumber firstChar pos acc cc =
        let val valid = explode ".+-e"
            fun lexNumber' pos digits [] = (rev digits, [], pos)
              | lexNumber' pos digits (x :: xs) =
                if Char.isDigit x orelse List.exists (fn c => x = c) valid
                then lexNumber' (pos + 1) (x :: digits) xs
                else (rev digits, x :: xs, pos)
            val (digits, rest, newpos) = lexNumber' pos [] (firstChar :: cc)
        in
            case Real.fromString (implode digits) of
                NONE => error pos "Invalid number"
              | SOME r => lex newpos (T.NUMBER r :: acc) rest
        end
                                           
    and lex pos acc [] = OK (rev acc)
      | lex pos acc (x::xs) = 
        (case x of
             #" "  => lex
           | #"\t" => lex
           | #"\n" => lex
           | #"\r" => lex
           | #"{"  => lexChar T.CURLY_L
           | #"}"  => lexChar T.CURLY_R
           | #"["  => lexChar T.SQUARE_L
           | #"]"  => lexChar T.SQUARE_R
           | #":"  => lexChar T.COLON
           | #","  => lexChar T.COMMA
           | #"\"" => lexString
           | #"t"  => lexTrue
           | #"f"  => lexFalse
           | #"n"  => lexNull
           | x     => lexNumber x) (pos + 1) acc xs

    fun show [] = "end of input"
      | show (tok :: _) = T.toString tok

    fun parseObject (T.CURLY_R :: xs) = OK (OBJECT [], xs)
      | parseObject tokens =
        let fun parsePair (T.STRING label :: T.COLON :: xs) =
                (case parseTokens xs of
                     ERROR e => ERROR e
                   | OK (j, xs) => OK ((label, j), xs))
              | parsePair other =
                ERROR ("Object name/value pair expected before " ^ show other)
            fun parseObject' acc [] = ERROR "End of input during object"
              | parseObject' acc tokens =
                case parsePair tokens of
                    ERROR e => ERROR e
                  | OK (pair, T.COMMA :: xs) => parseObject' (pair :: acc) xs
                  | OK (pair, T.CURLY_R :: xs) => OK (OBJECT (pair :: acc), xs)
                  | OK (_, _) => ERROR "Expected , or } after object element"
        in
            parseObject' [] tokens
        end

    and parseArray (T.SQUARE_R :: xs) = OK (ARRAY [], xs)
      | parseArray tokens =
        let fun parseArray' acc [] = ERROR "End of input during array"
              | parseArray' acc tokens =
                case parseTokens tokens of
                    ERROR e => ERROR e
                  | OK (j, T.COMMA :: xs) => parseArray' (j :: acc) xs
                  | OK (j, T.SQUARE_R :: xs) => OK (ARRAY (rev (j :: acc)), xs)
                  | OK (_, _) => ERROR "Expected , or ] after array element"
        in
            parseArray' [] tokens
        end

    and parseTokens [] = ERROR "Value expected"
      | parseTokens (tok :: xs) =
        (case tok of
             T.NUMBER r => OK (NUMBER r, xs)
           | T.STRING s => OK (STRING s, xs)
           | T.BOOL b   => OK (BOOL b, xs)
           | T.NULL     => OK (NULL, xs)
           | T.CURLY_L  => parseObject xs
           | T.SQUARE_L => parseArray xs
           | _ => ERROR ("Unexpected token " ^ T.toString tok ^
                         " before " ^ show xs))
                                   
    fun parse str =
        case lex 1 [] (explode str) of
           ERROR e => ERROR e
         | OK tokens => case parseTokens tokens of
                            OK (value, []) => OK value
                          | OK (_, _) => ERROR "Extra data after input"
                          | ERROR e => ERROR e

end

