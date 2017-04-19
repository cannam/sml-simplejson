(* An almost-RFC-compliant minimal JSON parser with no dependency on
   anything outside the Basis library.

   Notes:

   * Code size is more important here than performance, and this is
     neither particularly fast nor suitable for very large files

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
              | lexString' pos text escaped (x::xs) =
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
                         #"\"" => OK (rev text, xs)
                       | #"\\" => lexString' (pos + 1) text true xs
                       | _     => lexString' (pos + 1) (x :: text) false xs
        in
            case lexString' pos [] false cc of
                OK (text, rest) =>
                lex (pos + 1) (T.STRING (implode text) :: acc) rest
              | ERROR e => ERROR e
        end
            
    and lexNumber firstChar pos acc cc = error pos "lexNumber not implemented"
                                           
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

    fun show [] = " end of input"
      | show (tok::_) = T.toString tok
        
    fun parseObject acc [] = ERROR "Object contents expected"
      | parseObject acc (T.CURLY_R :: rest) = OK (OBJECT acc, rest)
      | parseObject acc (tok :: rest) = ERROR "parseObject not implemented"

    and parseArray acc [] = ERROR "Array contents expected"
      | parseArray acc (T.SQUARE_R :: rest) = OK (ARRAY (rev acc), rest)
      | parseArray acc tokens =
        case parseTokens tokens of
            ERROR e => ERROR e
          | OK (json, T.COMMA :: rest) => parseArray (json :: acc) rest
          | OK (json, T.SQUARE_R :: rest) => OK (ARRAY (rev (json :: acc)),
                                                 rest)
          | OK (_, _) => ERROR "Expected , or ] after array element"

    and parseTokens [] = ERROR "Value expected"
      | parseTokens (tok::rest) =
        (case tok of
             T.NUMBER r => OK (NUMBER r, rest)
           | T.STRING s => OK (STRING s, rest)
           | T.BOOL b   => OK (BOOL b, rest)
           | T.NULL     => OK (NULL, rest)
           | T.CURLY_L  => parseObject [] rest
           | T.SQUARE_L => parseArray [] rest
           | _ => ERROR ("Unexpected token " ^ (T.toString tok) ^ " before " ^
                         (show rest)))
                                   
    fun parse str =
        case lex 1 [] (explode str) of
           ERROR e => ERROR e
         | OK tokens => case parseTokens tokens of
                            OK (value, []) => OK value
                          | OK (_, _) => ERROR "Extra data after input"
                          | ERROR e => ERROR e

end

(* val test = JsonParser.parse "{\n    \"id\": \"http://vamp-plugins.org/piper/json/schema/loadrequest#\",\n    \"$schema\": \"http://json-schema.org/draft-04/schema#\",\n    \"description\": \"schema for a request to load a feature extractor; may be served in the params field of a load-method rpcrequest\",\n    \"type\": \"object\",\n    \"properties\": {\n	\"key\": {\n	    \"type\": \"string\"\n	},\n	\"inputSampleRate\": {\n	    \"type\": \"number\"\n	},\n	\"adapterFlags\": {\n	    \"type\": \"array\",\n	    \"items\": {\n		\"$ref\": \"http://vamp-plugins.org/piper/json/schema/enums#/definitions/adapter_flags\"\n	    }\n	}\n    },\n    \"required\": [ \"key\", \"inputSampleRate\" ],\n    \"additionalProperties\": false\n}" *)


