module K = Kittyimg
module RC = Rowcolumns

exception Too_many_images

module Ids = struct
  type t = Notty.A.color

  let state = ref (0,0,0)

  let incr () =
    let (r,g,b) = !state in
    let b,g = if b = 5 then 0, g + 1 else b + 1, g in
    let g,r = if g > 5 then 0, r + 1 else g, r in
    if r > 5 then
      raise Too_many_images
    else
      state := (r,g,b)

  let fresh () =
    let (r,g,b) = !state in
    let id = Notty.A.rgb ~r ~g ~b in
    incr ();
    id

  let to_kittyid i = K.Id.of_int (Obj.magic i land 0xff)
end

let placeholder = Uchar.of_int 0x10EEEE

external wsize : Unix.file_descr -> int * int * int * int =
  "caml_kittyimg_winsize"

(* Haaaaacky *)
let winsize = wsize (Unix.descr_of_out_channel stdout)

let width_to_cols iw =
  let (c,_,w,_) = winsize in
  Float.round (float iw /. (float w /. float c))
  |> Float.to_int

let height_to_rows ih =
  let (_,r,_,h) = winsize in
  Float.round (float ih /. (float h /. float r))
  |> Float.to_int

let of_string ~w ~h ~format data =
  let id = Ids.fresh () in
  (* Load image *)
  K.send_image ~w ~h ~format ~quiet:`OK ~mode:(`Store (Ids.to_kittyid id)) data;
  (* Create virtual placement *)
  let c = width_to_cols w in
  let r = height_to_rows h in
  let opts =
    K.display_opts ~virtual_placement:() ~cstretch:c ~rstretch:r
      ~quiet:`Failure ()
  in
  K.display_image ~opts (Ids.to_kittyid id);
  (* Create placeholder as a notty I.t *)
  let id_as_attr = Notty.A.fg id in
  let rows =
    List.init r (fun row ->
        let row = RC.diacritics.(row) in
        let line = Array.make (c * 3) placeholder in
        for i = 0 to c - 1 do
          line.(i * 3 + 1) <- row;
          line.(i * 3 + 2) <- RC.diacritics.(i);
        done;
        Notty.I.uchars id_as_attr line
      )
  in
  Notty.I.vcat rows
