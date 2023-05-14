type t = { command : string * string }

exception MalformedInput

let parse inp = 
  if (inp |> String.lowercase_ascii |> String.trim) = "quit" 
    then {command = ("quit", "game")}
  else 
  if (inp |> String.lowercase_ascii |> String.trim) = "u" 
    then {command = ("u", "u")}
  else 
  let sq_list = String.split_on_char ' ' (String.trim inp) in
  if (List.length sq_list) <> 2 then raise MalformedInput else
    match sq_list with
    | a :: b :: [] -> 
      if (String.length a <> 2) || (String.length b <> 2) 
        then raise MalformedInput 
      else 
        let sq1l = Char.lowercase_ascii (String.get a 0) in
        let sq1n = String.get a 1 in
        let sq2l = Char.lowercase_ascii (String.get b 0) in 
        let sq2n = String.get b 1 in if
        (Char.code sq1l - 97 < 0) || 
        (Char.code sq1l - 97 > 7) || 
        (Char.code sq2l - 97 < 0) ||
        (Char.code sq2l - 97 > 7) ||
        (Char.code sq1n - 49 < 0) ||
        (Char.code sq1n - 49 > 7) ||
        (Char.code sq2n - 49 < 0) ||
        (Char.code sq2n - 49 > 7) then raise MalformedInput else
          let sq1 = (Char.escaped sq1l) ^ (Char.escaped sq1n) in
          let sq2 = (Char.escaped sq2l) ^ (Char.escaped sq2n) in
          if sq1 = sq2 then raise MalformedInput 
          else
          {command = (sq1,sq2)}
    | _ -> raise MalformedInput

let get_command cmd = 
  let (a,b) = cmd.command in (a ^ " " ^ b)
