open System
let rec summ list = 
    match list with
    | [] -> 0
    | h::t -> let tail = summ t
              h + tail
////////
let rec max h t =
    match t with
    | [] -> h
    | head::tail -> if (head > h) then max head tail
                    else max h tail
////////
let rec new_list list ma mi = 
    match list with
    | [] -> []
    | h::t -> let tail = new_list t ma mi
              if ((h > mi) && (h < ma)) then h::tail
              else tail
////////
let rec read_list n = 
    if n = 0 then []
    else Convert.ToInt32(Console.ReadLine())::read_list(n-1)
////////
let middle list size =
    match list with
    | [] -> 0
    | h::t -> summ list / size
////////
let rec write_list list = 
    match list with
    |[] -> Console.Write("\n")
           let z = Console.ReadKey()
           0
    |h::t -> Console.Write(h.ToString())
             Console.Write("   ")
             write_list t
////////
let it_start list size = 
    match list with
    | [] -> []
    | h::t -> Console.Write("Результирующий список --  ")
              new_list list (max h t) (middle list size)
////////
[<EntryPoint>]
let main argv = 
    Console.Write("Введите размер списка: ")
    let size = Convert.ToInt32(Console.ReadLine())
    Console.Write("Введите список из ")
    Console.Write(size.ToString())
    Console.Write(" элементов\n")
    write_list(it_start(read_list size) size)