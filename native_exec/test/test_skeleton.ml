let%expect_test "native_exec links" =
  print_int Native_exec.page_size;
  [%expect {| 4096 |}]
