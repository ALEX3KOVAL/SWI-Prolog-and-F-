open System
open System.Drawing
open System.IO
open System.Windows.Forms
open System.Runtime.InteropServices

let rec readlist b list = function
    "" when (b = "") -> list
    | "" when (b <> "") -> Convert.ToInt32(b)::list
    | n when (n.[0] = ' ') -> readlist "" (Convert.ToInt32(b)::list) (n.Remove(0,1))
    | s when (s.[0] <> ' ')-> readlist (b + (s.[0].ToString())) list (s.Remove(0,1))

let form = new Form (Width = 430, Height = 300, Text = "Главная форма")
let button = new Button(Text = "Нажмите", Top = 210,Left = 10)
let box1 = new RichTextBox(Width = 400, Height = 30, Font = new Font(FontFamily("Consolas"), 12.0f,FontStyle.Bold), Top = 50)
let box2 = new RichTextBox(Width = 400, Height = 30, Font = new Font(FontFamily("Consolas"), 12.0f,FontStyle.Bold), Top = 148)
let inp = new Label(Text = "INPUT:", Top = 5, ForeColor = Color.Red, Font = new Font(FontFamily("Times New Roman"), 17.0f, FontStyle.Regular))
inp.Width <- 400
form.Controls.Add(inp)
box1.Dock = DockStyle.Fill
box2.Dock = DockStyle.Fill
form.Controls.Add(box1)
form.Controls.Add(button)
let ind13 =
    button.MouseClick
    |> Observable.map(fun clickArgs -> let str = box1.Text
                                       let lt = readlist "" [] str
                                       let list = List.sort lt
                                       let rec loop1 list (tmp:(int*int*int) list) = function
                                            x when (x < List.length list) -> let rec loop2 list tmp n = function
                                                                                j when (j < List.length list) -> let rec loop3 list tmp n n1 = function
                                                                                                                    ind when ((ind < List.length list) && (((List.item ind list) * (List.item ind list)) + ((List.item n1 list) * (List.item n1 list)) = ((List.item n list) * (List.item n list))) && ((List.item ind list) < (List.item n1 list))) -> if (List.isEmpty tmp) then loop3 list (([(List.item ind list,List.item n1 list, List.item n list)]):(int * int * int) list) n n1 (ind+1:int)
                                                                                                                                                                                                                                                                                                                                                        else let ele = (List.item ind list, List.item n1 list, List.item n list)
                                                                                                                                                                                                                                                                                                                                                             if (List.exists (fun elem -> elem = ele) tmp) then loop3 list tmp n n1 (ind+1)
                                                                                                                                                                                                                                                                                                                                                             elif (List.exists (fun elem -> let (ft,_,_) = elem
                                                                                                                                                                                                                                                                                                                                                                                            let (_,sd,_) = elem
                                                                                                                                                                                                                                                                                                                                                                                            let (ft1,_,_) = ele
                                                                                                                                                                                                                                                                                                                                                                                            let (_,sd1,_) = ele
                                                                                                                                                                                                                                                                                                                                                                                            (ft + sd) = (ft1 + sd1)) tmp) then loop3 list tmp n n1 (ind+1:int)
                                                                                                                                                                                                                                                                                                                                                             else loop3 list ((List.append tmp [ele]):(int * int * int) list) n n1 (ind+1:int)
                                                                                                                    | ins when ins < List.length list -> loop3 list tmp n n1 (ins+1)
                                                                                                                    | s when (s = List.length list) -> (tmp:(int * int * int) list)
                                                                                                                 let tmp1 = loop3 list tmp n j 0
                                                                                                                 loop2 list tmp1 n (j+1)
                                                                                | v when (v = List.length list) -> (tmp:(int * int * int) list)
                                                                             let (tmp2:(int * int * int) list) = loop2 list tmp (x:int) 0
                                                                             loop1 list tmp2 (x+1)
                                            | c when (c = List.length list) -> (tmp:(int * int * int) list)                                                                                                                                                                                                          
                                       let g = loop1 list ([]:(int * int * int) list) 0
                                       box2.AppendText(g.ToString())
                                       ())
ind13
|> Observable.add(fun x -> let outp = new Label(Text = "OUTPUT:", Top = 97, ForeColor = Color.Red, Font = new Font(FontFamily("Times New Roman"), 17.0f, FontStyle.Regular))
                           outp.Width <- 401
                           form.Controls.Add(outp)
                           form.Controls.Add(box2))
do Application.Run(form)