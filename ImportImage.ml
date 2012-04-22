(* Import Image 
 * A helper module for importing images. Take an image in raw RGB format,
 * and converts it to a color array array, which can then be displayed by
 * the standard Graphics module of Ocaml. *)
module ImportImage =
struct
  let import_image filename : int array array = 
    let color_matrix = Array.make_matrix 12 12 0 in
    let chan = open_in filename in
    begin
      (try
        let counter1 = ref 0 in
          while !counter1 < 12 do
            let counter2 = ref 0 in
            while !counter2 < 12 do 
              begin 
                let r = int_of_char(input_char chan) in
                let g = int_of_char(input_char chan) in
                let b = int_of_char(input_char chan) in
                color_matrix.(!counter1).(!counter2) <- (Graphics.rgb r g b);
                incr counter2
              end
            done;
            incr counter1
          done
      with End_of_file -> close_in chan);
      close_in chan;
      color_matrix
    end
end
