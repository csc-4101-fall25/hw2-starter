

open OUnit2
open Basics

(* abs tests: negative, zero, large positive *)
let test_abs _ =
  assert_equal 42 (abs (-42)) ~msg:"abs (negative)";
  assert_equal 0 (abs 0) ~msg:"abs (zero)";
  assert_equal 123456 (abs 123456) ~msg:"abs (large positive)"

(* rev_tup: different types, same values, negative numbers *)
let test_rev_tup _ =
  assert_equal ("b", "a") (rev_tup ("a", "b")) ~msg:"rev_tup (string)";
  assert_equal (5.0, -3.2) (rev_tup (-3.2, 5.0)) ~msg:"rev_tup (float)";
  assert_equal (7, 7) (rev_tup (7, 7)) ~msg:"rev_tup (same values)"

(* rev_triple: all same, floats, mixed types *)
let test_rev_triple _ =
  assert_equal (1, 1, 1) (rev_triple (1, 1, 1)) ~msg:"rev_triple (all same)";
  assert_equal (3.0, 2.0, 1.0) (rev_triple (1.0, 2.0, 3.0)) ~msg:"rev_triple (floats)";
  assert_equal ('z', 0, "hi") (rev_triple ("hi", 0, 'z')) ~msg:"rev_triple (mixed types)"

(* is_odd: negative odd, negative even, large odd *)
let test_is_odd _ =
  assert_equal true (is_odd (-7)) ~msg:"is_odd (negative odd)";
  assert_equal false (is_odd (-8)) ~msg:"is_odd (negative even)";
  assert_equal true (is_odd 99999) ~msg:"is_odd (large odd)"

(* is_older: same date, same year/month different day, same year different month *)
let test_is_older _ =
  assert_equal false (is_older (2022, 5, 10) (2022, 5, 10)) ~msg:"is_older (same date)";
  assert_equal true (is_older (2022, 5, 9) (2022, 5, 10)) ~msg:"is_older (same year/month, older day)";
  assert_equal true (is_older (2022, 4, 15) (2022, 5, 1)) ~msg:"is_older (same year, older month)"

(* to_us_format: leap year, single digit month/day, all same *)
let test_to_us_format _ =
  assert_equal (2, 29, 2020) (to_us_format (2020, 2, 29)) ~msg:"to_us_format (leap year)";
  assert_equal (3, 7, 2001) (to_us_format (2001, 3, 7)) ~msg:"to_us_format (single digit)";
  assert_equal (1, 1, 1) (to_us_format (1, 1, 1)) ~msg:"to_us_format (all ones)"

(* pow: negative exponent (should not loop forever), one as base, zero as base *)
let test_pow _ =
  assert_equal 1 (pow 1 100) ~msg:"pow (one as base)";
  assert_equal 0 (pow 0 5) ~msg:"pow (zero as base)";
  (* negative exponent not supported, but let's check p=0 for negative base *)
  assert_equal 1 (pow (-7) 0) ~msg:"pow (negative base, zero power)"

(* fac: 1, 6, 10 *)
let test_fac _ =
  assert_equal 1 (fac 1) ~msg:"fac (one)";
  assert_equal 720 (fac 6) ~msg:"fac (six)";
  assert_equal 3628800 (fac 10) ~msg:"fac (ten)"

(* get_nth: first, last, out of bounds negative *)
let test_get_nth _ =
  assert_equal "first" (get_nth (0, ["first"; "second"; "third"])) ~msg:"get_nth (first)";
  assert_equal "third" (get_nth (2, ["first"; "second"; "third"])) ~msg:"get_nth (last)";
  assert_raises (Failure "Index out of bounds") (fun () -> get_nth (-1, ["a"; "b"]))

(* larger: both empty, equal length, negative numbers *)
let test_larger _ =
  assert_equal [] (larger [] []) ~msg:"larger (both empty)";
  assert_equal [1;2] (larger [1;2] [3;4]) ~msg:"larger (equal length, first returned)";
  assert_equal [-1;-2;-3] (larger [-1;-2;-3] [-4]) ~msg:"larger (negatives)"

(* sum: negative numbers, one list longer, both negative *)
let test_sum _ =
  assert_equal (-6) (sum [-1;-2;-3] []) ~msg:"sum (all negative, one empty)";
  assert_equal 6 (sum [1;2;3] [0;0]) ~msg:"sum (one longer)";
  assert_equal (-10) (sum [-1;-2] [-3;-4]) ~msg:"sum (both negative)"

let suite =
  "student" >::: [
    "abs" >:: test_abs;
    "rev_tup" >:: test_rev_tup;
    "rev_triple" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "is_older" >:: test_is_older;
    "to_us_format" >:: test_to_us_format;
    "pow" >:: test_pow;
    "fac" >:: test_fac;
    "get_nth" >:: test_get_nth;
    "larger" >:: test_larger;
    "sum" >:: test_sum
  ]

let _ = run_test_tt_main suite
