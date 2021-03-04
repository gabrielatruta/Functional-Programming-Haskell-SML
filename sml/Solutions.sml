(*
-----------------------
-- Gabriela Truta
-- 29.02.2020
-----------------------
*)

fun min3 (no1: int) (no2: int) (no3: int): int = 
  Int.min (Int.min(no1, no2), no3);

fun levenshtein (s1: string) (s2: string): int =
  let
    fun helper size1 size2 =
      if (size1 = 0) then 
        size2
      else if (size2 = 0) then
        size1
      else if (List.nth (String.explode(s1), size1 - 1) = List.nth (String.explode(s2), size2 - 1)) then
        helper (size1 - 1) (size2 - 1)
      else
        1 + min3 (helper size1 (size2 - 1)) (helper (size1 - 1) size2) (helper (size1 - 1) (size2 - 1))
  in
    helper (List.length(String.explode(s1))) (List.length(String.explode(s2)))
end;