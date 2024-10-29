open Recognizer

let%test "" = chl_of_string "11001100" |> belongsTo = [true;false;false;false;true]
let%test "" = chl_of_string "0111" |> belongsTo = [true;true;false;false;false]
let%test "" = chl_of_string "100010" |> belongsTo = [true;false;false;true;false]
let%test "" = chl_of_string "001100" |> belongsTo = [true;false;true;true;true]