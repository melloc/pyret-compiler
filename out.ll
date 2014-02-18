@num.v7 = private unnamed_addr constant [2 x i8] c"7\00"
@num.v8 = private unnamed_addr constant [2 x i8] c"8\00"
define i64 @main() {
%x = alloca %struct.pyret-number*
%id.v1 = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([2 x i8]* @num.v7, i32 0, i32 0))
store %struct.pyret-number* %id.v1, %struct.pyret-number** %x
%y = alloca %struct.pyret-number*
%id.v2 = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([2 x i8]* @num.v8, i32 0, i32 0))
store %struct.pyret-number* %id.v2, %struct.pyret-number** %y
%z = alloca %struct.pyret-number*
%id.v4 = load %struct.pyret-number** %x
%id.v5 = load %struct.pyret-number** %y
%id.v6 = call %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %id.v4, %struct.pyret-number* %id.v5)
store %struct.pyret-number* %id.v6, %struct.pyret-number** %z
%id.v7 = load %struct.pyret-number** %z
call void @print-pyret-number(%struct.pyret-number* %id.v7)
ret i64 0
}