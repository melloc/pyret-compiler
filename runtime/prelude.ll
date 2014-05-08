target triple = "x86_64-pc-linux-gnu"

;; libc functions that we'll want
declare noalias i8* @malloc(i64) nounwind
declare i32 @fprintf(%struct._IO_FILE*, i8*, ...)
declare void @exit(i32) noreturn nounwind

;; needed intrinsics
declare i8* @llvm.adjust.trampoline(i8*)
declare void @llvm.init.trampoline(i8*, i8*, i8*)

%struct.pyret-value = type { i32, i32, i8* }
%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@stderr = external global %struct._IO_FILE*
@global.empty-table = internal global i8* null

;; Simple main function for now
define i64 @main(i32  %argc, i8**  %argv) {
    call %struct.pyret-value @init(i32 %argc, i8** %argv)
    ret i64 0
}
