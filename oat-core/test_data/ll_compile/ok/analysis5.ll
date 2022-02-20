define i64 @program(i64 %argc, i8** %argv) {
  %1 = add i64 5, 2
  %2 = add i64 2, 5
  %3 = mul i64 %1, 2
  call void @assert_int_equal(i64 %3, i64 14)
  ret i64 0
}
