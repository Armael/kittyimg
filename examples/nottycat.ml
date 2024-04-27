let icat filename =
  let img = Result.get_ok (Stb_image.load ~channels:4 filename) in
  let img_s = Kittyimg.string_of_bytes_ba img.Stb_image.data in
  let img =
    Nottyimg.of_string ~w:img.Stb_image.width ~h:img.Stb_image.height
      ~format:`RGBA img_s
  in
  let open Notty in
  let blanks n = I.(vcat @@ List.init n (fun _ -> string A.empty "")) in
  I.((img <-> blanks 2 <-> string A.empty "Woop woop!") <|> (blanks 4 <-> img))
  |> Notty_unix.output_image

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ filename ] -> icat filename
  | _ ->
    Printf.eprintf "usage: %s <imagefile>\n" Sys.argv.(0); exit 1
