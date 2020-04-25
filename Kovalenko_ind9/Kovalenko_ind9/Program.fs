 let rec total (number:uint32) (divider:uint32) (sum:uint32):uint32 = 
    if (number <= divider) then sum
    elif ((number % divider) = 0u) then total (number) (divider+1u) (sum+divider)
    else total number (divider+1u) sum
 
 let rec friendly_numbers (count:uint32) (number:uint32):uint32 = 
    let summ = total number 2u 1u
    if (number = 10000u) then count
    else if ((number = total summ 2u 1u) && (number <> summ)) then 
        friendly_numbers (count+1u) (number+1u)
    else friendly_numbers count (number+1u)

 [<EntryPoint>]
 let main argv = 
    let amount = friendly_numbers 0u 2u
    System.Console.Write("The number of all pairs of friendly numbers not exceeding 10000 equals ")
    System.Console.WriteLine(amount/2u)
    let ch = System.Console.ReadKey()
    0