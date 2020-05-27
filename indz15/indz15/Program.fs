type characteristics = 
    | ID of string
    | Пол of string
    | Рост of int
    | Вес of float32
    | Возраст of int
    | AMR of int            // Active Metabolic Rate (энергия, необходимая для физической активности)
    | BMR of string * float32 * float32 * int        // Basal Metabolic Rate (энергия, необходимая для поддержания жизни)
    | Ккал of float32 * float32
    | Жиры of float32
    | Белки of float32
    | Углеводы of float32
    member this.values:int = 
        match this with
            | Возраст(x) -> x
            | Рост(x) -> x
    member this.str:string = 
        match this with
            ID(x) -> x
            | Пол(x) -> x
    member this.calc:float32 = 
        match this with
            | Вес(x) -> x
            | AMR(activity) when activity = 0 -> 1.2f
            | AMR(activity) when (activity >= 1 && activity <= 3) -> 1.375f
            | AMR(activity) when (activity = 4 || activity = 5) -> 1.55f
            | AMR(activity) when (activity = 6 || activity = 7) -> 1.725f
            | AMR(activity) when activity > 7 -> 1.9f
            | BMR(gender, weight, height, age) when gender = "Male" -> (88.36f + ((13.4f * weight) + (4.8f * (float32)height) - (5.7f * (float32)age)))
            | BMR(gender, weight, height, age) when gender = "Female" -> (447.6f + ((9.2f * weight) + (3.1f * (float32)height) - (4.3f * (float32)age)))
            | Ккал(bmr,amr) -> bmr * amr
            | Жиры(kkal) -> kkal* 0.25f
            | Белки(weight) -> weight * 2.5f
            | Углеводы(kkal) -> kkal * 0.45f

type user = (characteristics * characteristics * characteristics * characteristics * characteristics * characteristics * characteristics * characteristics * characteristics * characteristics * characteristics)

let rec readstr str (fe:System.IO.StreamReader) = function
    124 -> str
    | 32 -> readstr str fe (fe.Read())
    | n -> readstr (str + ((char)n).ToString()) fe (fe.Read())

let rec schit (fs:System.IO.StreamReader) = function
    |10 -> ()
    | n -> schit fs (fs.Read())

let rec hat str (fs:System.IO.StreamReader) = function
    10 -> str
    | n -> hat (str + ((char)n).ToString()) fs (fs.Read())

let rec readall str (fe:System.IO.StreamReader) = function
     -1 -> str
     | n -> readall (str + ((char)n).ToString()) fe (fe.Read())

let rec writelist = function
        [] -> ()
        | (h:user)::(t:user list) ->  let (id,_,_,_,_,_,_,_,_,_,_) = h
                                      let (_,gender,_,_,_,_,_,_,_,_,_) = h
                                      let (_,_,height,_,_,_,_,_,_,_,_) = h
                                      let (_,_,_,weight,_,_,_,_,_,_,_) = h
                                      let (_,_,_,_,age,_,_,_,_,_,_) = h
                                      let (_,_,_,_,_,amr,_,_,_,_,_) = h
                                      let (_,_,_,_,_,_,bmr,_,_,_,_) = h
                                      let (_,_,_,_,_,_,_,kkal,_,_,_) = h
                                      let (_,_,_,_,_,_,_,_,fats,_,_) = h
                                      let (_,_,_,_,_,_,_,_,_,protein,_) = h
                                      let (_,_,_,_,_,_,_,_,_,_,carbo) = h
                                      System.Console.Write("{0,-6}|",id.str)
                                      System.Console.Write("{0,-6}|",gender.str)
                                      System.Console.Write("{0,-6}|",height.values.ToString())
                                      System.Console.Write("{0,-3}|",weight.calc.ToString())
                                      System.Console.Write("{0,-6}|",age.values.ToString())
                                      System.Console.Write("{0,-5}|",amr.calc.ToString())
                                      System.Console.Write("{0,-9}|",bmr.calc.ToString())
                                      System.Console.Write("{0,-9}|",kkal.calc.ToString())
                                      System.Console.Write("{0,-8}|",fats.calc.ToString())
                                      System.Console.Write("{0,-8}|",protein.calc.ToString())
                                      System.Console.WriteLine("{0,-13}|",carbo.calc.ToString())
                                      writelist t

let delinf (inf:string) (list:user list) = function
    [] -> list
    | (h:user)::(t:user list) -> List.filter (fun x -> let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                       id.str <> inf) (h::t)

[<EntryPoint>]
let main argv = 
    let rec menu (list:user list) (str:string) = function
        0 -> System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
             let answer = System.Console.ReadLine()
             menu list str ((int)answer)
        | 1 -> let fs = new System.IO.StreamReader(@"C:\Users\User\source\repos\indz15\bd.TXT")
               let rec add_customer (fs:System.IO.StreamReader) (list:user list) = function 
                   -1 -> System.Console.WriteLine("Данные закончились...")
                         System.Console.ReadKey()
                         System.Console.Clear()
                         list
                   | 10 -> list
                   | f -> let id = readstr "" fs f
                          let gender = readstr "" fs ((fs.Read()))
                          let height = System.Convert.ToInt32(readstr "" fs ((fs.Read())))
                          let weight = (float32)(System.Convert.ToDouble(readstr "" fs ((fs.Read()))))
                          let age = System.Convert.ToInt32(readstr "" fs ((fs.Read())))
                          let amr = System.Convert.ToInt32(readstr "" fs (fs.Read()))
                          if (fs.Peek() > -1) then schit fs (fs.Read())
                                                   let fo = readall "" fs (fs.Read())
                                                   fs.Close()
                                                   let sw = new System.IO.StreamWriter(@"C:\Users\User\source\repos\indz15\bd.TXT", false, System.Text.Encoding.Default)
                                                   sw.Write(fo)
                                                   sw.Close()
                          else  fs.Close()
                                let sw = new System.IO.StreamWriter(@"C:\Users\User\source\repos\indz15\bd.TXT", false, System.Text.Encoding.Default)
                                sw.Write("")
                                sw.Close()
                          if (List.isEmpty list = true) then let rt = [(ID(id), Пол(gender), Рост(height), Вес(weight), Возраст(age), AMR(amr), BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values),
                                                                        Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc), 
                                                                        Жиры(Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc).calc),
                                                                        Белки(Вес(weight).calc), Углеводы(Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc).calc))]
   
                                                             add_customer fs rt 10
                          else let lt = List.append list [(ID(id), Пол(gender), Рост(height), Вес(weight), Возраст(age), AMR(amr), BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values),
                                                           Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc), 
                                                           Жиры(Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc).calc),
                                                           Белки(Вес(weight).calc), Углеводы(Ккал(BMR(Пол(gender).str, Вес(weight).calc, (float32)(Рост(height)).values, Возраст(age).values).calc, AMR(amr).calc).calc))]
                               add_customer fs lt 10   
               let newlt = add_customer fs list (fs.Read())
               fs.Close()
               System.Console.WriteLine("Текущий список:\n")
               System.Console.WriteLine(str)
               writelist (newlt)
               System.Console.WriteLine("Нажмите любую клавишу для продолжения...")
               System.Console.ReadKey()
               System.Console.Clear()
               System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
               let ans = System.Console.ReadLine()
               menu newlt str ((int)ans)
        | 2 -> if (List.isEmpty list = true) then System.Console.WriteLine("Данных нет...\nНажмите любую клавишу для продолжения...")
                                                  System.Console.ReadKey()
                                                  System.Console.Clear()
                                                  System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
                                                  let ans = System.Console.ReadLine()
                                                  menu list str ((int)ans)
               else                                    
                    let red (list:user list) =
                        let redact (list:user list) inf = function
                            (1:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                if (ident.str = inf) then System.Console.Write("Введите новый ID:")
                                                                                          let change = System.Console.ReadLine()
                                                                                          let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                          let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                          let (_,_,_,weight,_,_,_,_,_,_,_) = x
                                                                                          let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                          let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                          let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                          let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                          let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                          let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                          let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                          System.Console.WriteLine("Текущий список:\n")
                                                                                          (ID(change),gender,height,weight,age,amr,bmr,kkal,fats,protein,carbo)
                                                                else x) list
                            | (2:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите текущий пол: ")
                                                                                            let change = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,weight,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x                                                                
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,Пол(change),height,weight,age,amr,bmr,kkal,fats,protein,carbo)
                                                                  else x) list
                            | (3:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите новый рост: ")
                                                                                            let change = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,weight,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,Рост((int)change),weight,age,amr,bmr,kkal,fats,protein,carbo)
                                                                  else x
                                                                  ) list
                            | (4:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите новый рост: ")
                                                                                            let change = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,height,Вес((float32)change),age,amr,bmr,kkal,fats,protein,carbo)
                                                                  else x
                                                                  ) list  
                            | (5:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите новый возраст: ")
                                                                                            let change = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,weight,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,height,weight,Возраст((int)change),amr,bmr,kkal,fats,protein,carbo)
                                                                  else x
                                                                  ) list
                            | (6:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.WriteLine("Введите новое количество тренировок в неделю: ")
                                                                                            let change = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,weight,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,height,weight,age,AMR((int)change),bmr,kkal,fats,protein,carbo)
                                                                  else x
                                                                  ) list
                            | (7:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите текущий вес: ")
                                                                                            let change1 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий рост: ")
                                                                                            let change2 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий возраст: ")
                                                                                            let change3 = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,Рост((int)change2),Вес((float32)change1),Возраст((int)change3),amr,BMR(gender.str,(float32)change1,(float32)change2,(int)change3),kkal,fats,protein,carbo)
                                                                  else x
                                                                  ) list
                            | (8:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите текущий AMR: ")
                                                                                            let change1 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий вес: ")
                                                                                            let change2 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий рост: ")
                                                                                            let change3 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий возраст: ")
                                                                                            let change4 = System.Console.ReadLine()   
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,Рост((int)change3),Вес((float32)change2),Возраст((int)change4),AMR((int)change1),BMR(gender.str,(float32)change2,(float32)change3,(int)change4),Ккал(BMR(gender.str,(float32)change2,(float32)change3,(int)change4).calc,(float32)change1),fats,protein,carbo)                                                                                     
                                                                  else x
                                                                  ) list
                            | (9:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                  if (ident.str = inf) then System.Console.Write("Введите текущий AMR: ")
                                                                                            let change1 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий вес: ")
                                                                                            let change2 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий рост: ")
                                                                                            let change3 = System.Console.ReadLine()
                                                                                            System.Console.Write("Введите текущий возраст: ")
                                                                                            let change4 = System.Console.ReadLine()
                                                                                            let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                            let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                            System.Console.WriteLine("Текущий список:\n")
                                                                                            (id,gender,Рост((int)change3),Вес((float32)change2),Возраст((int)change4),AMR((int)change1),BMR(gender.str,(float32)change2,(float32)change3,(int)change4),Ккал(BMR(gender.str,(float32)change2,(float32)change3,(int)change4).calc,(float32)change1),Жиры(Ккал(BMR(gender.str,(float32)change2,(float32)change3,(int)change4).calc,(float32)change1).calc),protein,carbo)                                                    
                                                                  else x
                                                                  ) list
                            | (10:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                   if (ident.str = inf) then System.Console.Write("Введите текущий вес: ")
                                                                                             let change = System.Console.ReadLine()
                                                                                             let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                             let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                             let (_,_,height,_,_,_,_,_,_,_,_) = x
                                                                                             let (_,_,_,_,age,_,_,_,_,_,_) = x
                                                                                             let (_,_,_,_,_,amr,_,_,_,_,_) = x
                                                                                             let (_,_,_,_,_,_,bmr,_,_,_,_) = x
                                                                                             let (_,_,_,_,_,_,_,kkal,_,_,_) = x
                                                                                             let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                             let (_,_,_,_,_,_,_,_,_,_,carbo) = x
                                                                                             System.Console.WriteLine("Текущий список:\n")
                                                                                             (id, gender, height, Вес((float32)change), age, amr, bmr, kkal, fats, Белки((float32)change), carbo)
                                                                   else x
                                                                   ) list
                            | (11:int) -> List.map(fun (x:user) -> let (ident,_,_,_,_,_,_,_,_,_,_) = x
                                                                   if (ident.str = inf) then System.Console.Write("Введите текущий AMR: ")
                                                                                             let change1 = System.Console.ReadLine()
                                                                                             System.Console.Write("Введите текущий вес: ")
                                                                                             let change2 = System.Console.ReadLine()
                                                                                             System.Console.Write("Введите текущий рост: ")
                                                                                             let change3 = System.Console.ReadLine()
                                                                                             System.Console.Write("Введите текущий возраст: ")
                                                                                             let change4 = System.Console.ReadLine()
                                                                                             let (id,_,_,_,_,_,_,_,_,_,_) = x
                                                                                             let (_,gender,_,_,_,_,_,_,_,_,_) = x
                                                                                             let (_,_,_,_,_,_,_,_,fats,_,_) = x
                                                                                             let (_,_,_,_,_,_,_,_,_,protein,_) = x
                                                                                             System.Console.WriteLine("Текущий список:\n")
                                                                                             (id,gender,Рост((int)change3),Вес((float32)change2),Возраст((int)change4),AMR((int)change1),BMR(gender.str,(float32)change2,(float32)change3,(int)change4),Ккал(BMR(gender.str,(float32)change2,(float32)change3,(int)change4).calc,(float32)change1),fats, protein,Углеводы(Ккал(BMR(gender.str,(float32)change2,(float32)change3,(int)change4).calc,(float32)change1).calc))                                                                                      
                                                                   else x
                                                                   ) list
                        if (List.isEmpty list = false) then System.Console.Write("Под каким номером вы хотите отредактировать запись от 1 до ")
                                                            System.Console.Write(List.length list)
                                                            System.Console.Write(": ")
                                                            let ans = System.Console.ReadLine()
                                                            System.Console.Write("Выберите что вы хотите отредактировать:\n1. ID;\n2. Пол;\n3. Рост;\n4. Вес;\n5. Возраст;\n6. AMR;\n7. BMR;\n8. Ккал;\n9. Жиры;\n10. Белки;\n11. Углеводы.\n")
                                                            let ans1 = System.Console.ReadLine()
                                                            let (d,_,_,_,_,_,_,_,_,_,_) = List.item ((int)ans - 1:int) list
                                                            redact list d.str ((int)ans1) 
                        else System.Console.Write("База данных пуста...\nНажмите любую клавишу для продолжения:\n")
                             System.Console.ReadKey()
                             System.Console.Clear()
                             []
                    let ff = red list
                    System.Console.WriteLine(str)
                    writelist ff
                    System.Console.WriteLine("Нажмите любую клавишу для продолжения...")
                    System.Console.ReadKey()
                    System.Console.Clear()
                    System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
                    let ans = System.Console.ReadLine()
                    menu ff str ((int)ans)
        | 3 -> if (List.isEmpty list = true) then System.Console.WriteLine("Данных нет...\nНажмите любую клавишу для продолжения...")
                                                  System.Console.ReadKey()
                                                  System.Console.Clear()
                                                  System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
                                                  let ans = System.Console.ReadLine()
                                                  menu list str ((int)ans)
               else 
                    System.Console.Write("Под каким номером вы хотите удалить запись от 1 до ")
                    System.Console.Write(List.length list)
                    System.Console.Write(": ")
                    let ans = System.Console.ReadLine()
                    System.Console.WriteLine("Запись удалена.")
                    let (id,_,_,_,_,_,_,_,_,_,_) = List.item ((int)ans - 1:int) list
                    let newlist = delinf (id.str) ([]:user list) list
                    System.Console.WriteLine("Текущий список:\n")
                    if (List.isEmpty list = false) then System.Console.WriteLine(str)
                                                        writelist newlist
                    else System.Console.WriteLine(" пуст.")
                    System.Console.WriteLine("\nНажмите любую клавишу для продолжения...")
                    System.Console.ReadKey()
                    System.Console.Clear()
                    System.Console.Write("Меню:\n1. Добавить запись;\n2. Редактировать запись;\n3. Удалить запись;\n4. Выход.\nВыберите действие: ")
                    let ans1 = System.Console.ReadLine()
                    menu newlist str ((int)ans1)
        | 4 -> System.Console.Clear()
               0
    if (System.IO.File.Exists(@"C:\Users\User\source\repos\indz15\bd.TXT")) then let fs = new System.IO.StreamReader(@"C:\Users\User\source\repos\indz15\bd.TXT")
                                                                                 let str = hat "" fs (fs.Read())
                                                                                 //schit fs (fs.Read())
                                                                                 let fo = readall "" fs (fs.Read())
                                                                                 fs.Close()
                                                                                 let sw = new System.IO.StreamWriter(@"C:\Users\User\source\repos\indz15\bd.TXT", false, System.Text.Encoding.Default)
                                                                                 sw.Write(fo)
                                                                                 sw.Close()
                                                                                 menu ([]:user list) str 0
    else System.Console.WriteLine("NO!!")
         0