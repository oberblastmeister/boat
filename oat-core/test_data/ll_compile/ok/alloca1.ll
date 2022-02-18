define i64 @program(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  store i64 17, i64* %1
  %2 = load i64, i64* %1
  call void @assert_int_equal(i64 %2, i64 17)
  ret i64 0
}
