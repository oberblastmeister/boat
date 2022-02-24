define i64 @test() {
    %1 = add i64 1, 1
    %2 = add i64 %1, 1234
    br label %l3
l3:
    %4 = add i64 %2, 4
    ret i64 %4
}
