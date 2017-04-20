
(* Take an input file and pass it through the JSON parser and
   serialiser, writing the result to stdout. Used for integrity
   checks. *)

fun contents filename =
    let val stream = TextIO.openIn filename
        fun read_all str acc =
            case TextIO.inputLine str of
                SOME line => read_all str (line :: acc)
              | NONE => rev acc
        val contents = read_all stream []
        val _ = TextIO.closeIn stream
    in
        String.concatWith "\\n" contents
    end

fun processFile filename =
    let open Json
        val input = contents filename
    in
        case parse input of
            ERROR e => TextIO.output (TextIO.stdErr, "Error: " ^ e ^ "\n")
          | OK json => print (serialise json ^ "\n")
    end
        
fun main () =
    case CommandLine.arguments () of
        [infile] => processFile infile
      | _ => (TextIO.output (TextIO.stdErr, "Usage: test file.json\n");
              raise Fail "Incorrect arguments specified")

                 
