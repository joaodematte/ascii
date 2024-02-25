let ascii =
  [ 0, "NUL"
  ; 1, "SOH"
  ; 2, "STX"
  ; 3, "ETX"
  ; 4, "EOT"
  ; 5, "ENQ"
  ; 6, "ACK"
  ; 7, "BEL"
  ; 8, "BS"
  ; 9, "HT"
  ; 10, "LF"
  ; 11, "VT"
  ; 12, "FF"
  ; 13, "CR"
  ; 14, "SO"
  ; 15, "SI"
  ; 16, "DLE"
  ; 17, "DC1"
  ; 18, "DC2"
  ; 19, "DC3"
  ; 20, "DC4"
  ; 21, "NAK"
  ; 22, "SYN"
  ; 23, "ETB"
  ; 24, "CAN"
  ; 25, "EM"
  ; 26, "SUB"
  ; 27, "ESC"
  ; 28, "FS"
  ; 29, "GS"
  ; 30, "RS"
  ; 31, "US"
  ; 32, " "
  ; 33, "!"
  ; 34, "\""
  ; 35, "#"
  ; 36, "$"
  ; 37, "%"
  ; 38, "&"
  ; 39, "'"
  ; 40, "("
  ; 41, ")"
  ; 42, "*"
  ; 43, "+"
  ; 44, ","
  ; 45, "-"
  ; 46, "."
  ; 47, "/"
  ; 48, "0"
  ; 49, "1"
  ; 50, "2"
  ; 51, "3"
  ; 52, "4"
  ; 53, "5"
  ; 54, "6"
  ; 55, "7"
  ; 56, "8"
  ; 57, "9"
  ; 58, ":"
  ; 59, ";"
  ; 60, "<"
  ; 61, "="
  ; 62, ">"
  ; 63, "?"
  ; 64, "@"
  ; 65, "A"
  ; 66, "B"
  ; 67, "C"
  ; 68, "D"
  ; 69, "E"
  ; 70, "F"
  ; 71, "G"
  ; 72, "H"
  ; 73, "I"
  ; 74, "J"
  ; 75, "K"
  ; 76, "L"
  ; 77, "M"
  ; 78, "N"
  ; 79, "O"
  ; 80, "P"
  ; 81, "Q"
  ; 82, "R"
  ; 83, "S"
  ; 84, "T"
  ; 85, "U"
  ; 86, "V"
  ; 87, "W"
  ; 88, "X"
  ; 89, "Y"
  ; 90, "Z"
  ; 91, "["
  ; 92, "\\"
  ; 93, "]"
  ; 94, "^"
  ; 95, "_"
  ; 96, "`"
  ; 97, "a"
  ; 98, "b"
  ; 99, "c"
  ; 100, "d"
  ; 101, "e"
  ; 102, "f"
  ; 103, "g"
  ; 104, "h"
  ; 105, "i"
  ; 106, "j"
  ; 107, "k"
  ; 108, "l"
  ; 109, "m"
  ; 110, "n"
  ; 111, "o"
  ; 112, "p"
  ; 113, "q"
  ; 114, "r"
  ; 115, "s"
  ; 116, "t"
  ; 117, "u"
  ; 118, "v"
  ; 119, "w"
  ; 120, "x"
  ; 121, "y"
  ; 122, "z"
  ; 123, "{"
  ; 124, "|"
  ; 125, "}"
  ; 126, "~"
  ; 127, "DEL"
  ]
;;

let get_value int_value =
  try List.assoc int_value ascii with
  | Not_found -> "Unknown"
;;

let format_int i n =
  let string_number = Int.to_string i in
  String.make (n - String.length string_number) ' ' ^ string_number
;;

let format_string s n left =
  if left
  then String.make (n - String.length s) ' ' ^ s
  else s ^ String.make (n - String.length s) ' '
;;

let int_to_hex i = Printf.sprintf "%02X" i

let print_ascii_table =
  let lines = 15 in
  let columns = 7 in
  for k = 0 to columns do
    if k == columns
    then
      Fmt.pr
        "%s %s %s@."
        (format_string "DEC" 5 true)
        (format_string "HEX" 3 true)
        (String.make 3 ' ')
    else
      Fmt.pr
        "%s %s %s"
        (format_string "DEC" 5 true)
        (format_string "HEX" 3 true)
        (String.make 3 ' ')
  done;
  for i = 0 to lines do
    for j = 0 to columns do
      let entry = (16 * j) + i in
      Fmt.pr
        "%s %s %s"
        (format_int entry 5)
        (format_string (int_to_hex entry) 3 true)
        (format_string (get_value entry) 3 false)
    done;
    Fmt.pr "@."
  done
;;

let () = print_ascii_table
