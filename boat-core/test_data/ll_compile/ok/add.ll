define i64 @add_two(i64 %a, i64 %b) {
  %1 = add i64 %a, %b
  ret i64 %1
}

define i64 @program(i64 %argc, i8** %argv) {
  %1 = call i64 @add_two(i64 1, i64 2)
  call void @print_int(i64 %1)
  ret i64 0
}
