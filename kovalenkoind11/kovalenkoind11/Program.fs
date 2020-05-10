open System
open System.Drawing
open System.IO
open System.Windows.Forms
open System.Runtime.InteropServices
open System.Drawing

let form = new Form(Width= 700, Height = 480, Text = "Главная форма", Menu = new MainMenu())
let button = new Button(Text = "Нажмите", Top = 400)
form.Controls.Add(button)
let image = new PictureBox(SizeMode = PictureBoxSizeMode.Zoom,Size = new Size(250,250),Top = 5)
let rec slaid count =
    match count with
    | var when var < 9 -> let clickbut = 
                            button.MouseClick
                            |> Observable.map (fun clickArgs -> let string1 = "C:\Users\Александр\Pictures\pict"
                                                                let string2 = string1 + string((char)92) + string(count) + ".PNG" 
                                                                if (File.Exists(string2)) then 
                                                                    image.ImageLocation <- string2
                                                                else 
                                                                    let string3 = string2.TrimEnd('P','N','G') + "JPG"
                                                                    if (File.Exists(string3)) then
                                                                        image.ImageLocation <- string3
                                                                    else ())
                          clickbut
                          |> Observable.add(fun x -> form.Controls.Add(image)
                                                     slaid(count+1))
    | _ -> button.Hide()
           let label = new Label(Text = "КАРТИНКИ!" ,Top = 400, ForeColor = Color.Red, Font = new Font(FontFamily("Times New Roman"), 17.0f,FontStyle.Strikeout))
           label.Width <- 400
           form.Controls.Add(label)
slaid 0
do Application.Run(form)