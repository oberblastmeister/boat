define i64 @program(i64 %argc, i8** %arcv) {
  %1 = add i64 5, 9
  %2 = add i64 %1, 15
  call void @assert_int_equal(i64 %2, i64 29)
  ret i64 0
}
