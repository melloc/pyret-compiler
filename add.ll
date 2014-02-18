declare void @print-pyret-number(%struct.pyret-number*)
@num.v7 = private unnamed_addr constant [2 x i8] c"7\00"
@num.v8 = private unnamed_addr constant [2 x i8] c"8\00"
define i64 @main() {
%x = alloca %struct.pyret-number
%0 = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([2 x i8]* @num.v7, i32 0, i32 0))
store %struct.pyret-number %0, %struct.pyret-number* %x
%y = alloca %struct.pyret-number
%1 = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([2 x i8]* @num.v8, i32 0, i32 0))
store %struct.pyret-number %1, %struct.pyret-number* %y
%z = alloca %struct.pyret-number
%3 = load %struct.pyret-number* %x
%4 = load %struct.pyret-number* %y
%5 = call %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %3, %struct.pyret-number* %4)
store %struct.pyret-number %5, %struct.pyret-number* %z
%6 = load %struct.pyret-number* %z
call void @print-pyret-number(%struct.pyret-number* %6)
ret i64 0
}
