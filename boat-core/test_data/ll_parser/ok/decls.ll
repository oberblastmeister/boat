%disju = type { i64, i8* }
%var1 = type { i64, i64* }
%var2 = type { i64, i8** }
%test = type [10 x i8]

@gint = global i64 42
@v1 = global %var1 { i64 0, i64* @gint }
@v2 = global %var2 { i64 1, i8** null }
; @gstr = global [14 x i8] c"hello, world!\00"
