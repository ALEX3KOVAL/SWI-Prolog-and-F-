open System
open System.Drawing
open System.IO
open System.Windows.Forms
open System.Windows.Forms.VisualStyles

let form = new Form (Width = 370, Height = 300, Text = "Главная форма")
let button = new Button(Text = "Нажмите", Top = 190,Left = 10)
let box1 = new RichTextBox(Width = 240, Height = 30, Font = new Font(FontFamily("Consolas"), 16.0f,FontStyle.Bold), Top = 50)
let box2 = new RichTextBox(Width = 240, Height = 30, Font = new Font(FontFamily("Consolas"), 16.0f,FontStyle.Bold), Top = 100)
box1.Dock = DockStyle.Fill
box2.Dock = DockStyle.Fill
form.Controls.Add(box1)
form.Controls.Add(button)
let readstr =
    button.MouseClick
    |> Observable.map (fun ClickArgs -> let str = box1.Text
                                        let len = str.Length
                                        let rec sjatie stri tmp ch schet = function
                                            l when (str.[len - 1] = str.[len-2] && (l = len)) -> let str4 = tmp.ToString() + ch.ToString() + schet.ToString()
                                                                                                 if (str4.Length > stri.ToString().Length) then stri
                                                                                                 else str4
                                            | l when ((str.[len-1] <> str.[len-2]) && (l = len)) -> let str3 = tmp.ToString() + ch.ToString() + "1"
                                                                                                    if (str3.Length > stri.ToString().Length) then stri
                                                                                                    else str3
                                            | n when str.[n] = ch -> sjatie stri tmp ch (schet+1) (n+1)
                                            | n when str.[n] <> ch -> let str1 = tmp.ToString() + ch.ToString() + schet.ToString()
                                                                      sjatie stri str1 (str.[n]) 1 (n+1)
                                        let z = sjatie str "" (str.[0]) 1 1
                                        box2.AppendText(z.ToString()))
readstr
|> Observable.add(fun x -> form.Controls.Add(box2))
do Application.Run(form)