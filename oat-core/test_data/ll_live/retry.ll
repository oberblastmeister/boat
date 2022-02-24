define i64 @test(i64 %x) {
    %0 = add i64 1, 1
    br label %l0
l0:
    %1 = add i64 %x, 12
    %2 = add i64 1, 1
    br label %l1
l1:
    ; this is invalid
    ; ll is always in ssa-form
    ; this is just to test
    %1 = add i64 1, 1
    br label %l2
l2:
    %3 = add i64 1, 1
    br i1 1, label %l0, label %l3
l3:
    ret i64 %1
}
