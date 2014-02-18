target triple = "x86_64-pc-linux-gnu"

declare i64 @printf(i8*, i64)

@global_str = constant [4 x i8] c"%d\0a\00"
define i64 @main() {
	%temp = getelementptr [4 x i8]* @global_str, i64 0, i64 0
	call i64 @printf(i8* %temp, i64 56)
	ret i64 0
}
