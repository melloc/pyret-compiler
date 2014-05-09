
%struct.J_UDY_ERROR_STRUCT = type { i32, i32, [4 x i64] }
@judy.error.str1 = private constant [54 x i8] c"File '%s', line %d: %s(), JU_ERRNO_* == %d, ID == %d\0A\00"
@judy.error.str2 = private constant [4 x i8] c"a.c\00"
@judy.error.ins = private constant [9 x i8] c"JudyLIns\00"
@judy.error.get = private constant [9 x i8] c"JudyLGet\00"
@judy.error.next = private constant [10 x i8] c"JudyLNext\00"
@judy.error.first = private constant [11 x i8] c"JudyLFirst\00"

define void @check-judy(i8** %ret, i8* %function, %struct.J_UDY_ERROR_STRUCT* %J_Error) nounwind {
  ;; Perform some error checking
  %check-1 = bitcast i8** %ret to i8*
  %check-2 = bitcast i8* %check-1 to i64*
  %is-bad = icmp eq i64* %check-2, inttoptr (i64 -1 to i64*)
  br i1 %is-bad, label %error, label %good
error:
  %unload-stderr = load %struct._IO_FILE** @stderr
  %as = getelementptr inbounds %struct.J_UDY_ERROR_STRUCT* %J_Error, i32 0, i32 0
  %a = load i32* %as, align 4
  %bs = getelementptr inbounds %struct.J_UDY_ERROR_STRUCT* %J_Error, i32 0, i32 1
  %b = load i32* %bs, align 4
  call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %unload-stderr, i8* getelementptr inbounds ([54 x i8]* @judy.error.str1, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @judy.error.str2, i32 0, i32 0), i32 48, i8* %function, i32 %a, i32 %b)
  call void @exit(i32 1) noreturn
  unreachable
good:
  ret void
}

define i8* @table-copy(i8* %PJLArray) nounwind {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT
  %PValueA = alloca i8**
  %dst = alloca i8*
  store i8* null, i8** %dst
  %i = alloca i64
  store i64 0, i64* %i
  %first-ret = call i8** @JudyLFirst(i8* %PJLArray, i64* %i, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  call void @check-judy(i8** %first-ret, i8* getelementptr inbounds ([11 x i8]* @judy.error.first, i32 0, i32 0), %struct.J_UDY_ERROR_STRUCT* %J_Error)
  store i8** %first-ret, i8*** %PValueA
  br label %comparison

comparison:                                       ; preds = %copy-entry, %0
  %comparison-pval = load i8*** %PValueA
  %is-null = icmp ne i8** %comparison-pval, null
  br i1 %is-null, label %copy-entry, label %exit

copy-entry:                                       ; preds = %comparison
  %deref-i = load i64* %i
  %ins-ret = call i8** @JudyLIns(i8** %dst, i64 %deref-i, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  call void @check-judy(i8** %first-ret, i8* getelementptr inbounds ([9 x i8]* @judy.error.ins, i32 0, i32 0), %struct.J_UDY_ERROR_STRUCT* %J_Error)
  ;; Perform some error checking
  br label %copy-continue

copy-continue:
  %copy-entry-pval = load i8*** %PValueA
  %copy-entry-val = load i8** %copy-entry-pval
  store i8* %copy-entry-val, i8** %ins-ret
  %next-ret = call i8** @JudyLNext(i8* %PJLArray, i64* %i, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  call void @check-judy(i8** %next-ret, i8* getelementptr inbounds ([10 x i8]* @judy.error.next, i32 0, i32 0), %struct.J_UDY_ERROR_STRUCT* %J_Error)
  store i8** %next-ret, i8*** %PValueA
  br label %comparison

exit:                                      ; preds = %comparison
  %ret = load i8** %dst
  ret i8* %ret
}

define i8* @table-insert(i8** %PJLArray, i64 %key, i8* %to-insert) nounwind {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %ins-ret = call i8** @JudyLIns(i8** %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error) ;; Make a call to JudyLIns
  call void @check-judy(i8** %ins-ret, i8* getelementptr inbounds ([9 x i8]* @judy.error.ins, i32 0, i32 0), %struct.J_UDY_ERROR_STRUCT* %J_Error)
  store i8* %to-insert, i8** %ins-ret                       ;; Store pointer to a there. 
  ;; At some point, we should do some checking for errors here.
  %table-deref = load i8** %PJLArray
  ret i8* %table-deref
}

define i8* @table-lookup(i8* %PJLArray, i64 %key) alwaysinline {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %lookup-ret = call i8** @JudyLGet(i8* %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  call void @check-judy(i8** %lookup-ret, i8* getelementptr inbounds ([9 x i8]* @judy.error.get, i32 0, i32 0), %struct.J_UDY_ERROR_STRUCT* %J_Error)
  %deref = load i8** %lookup-ret                            ;; Dereference that.
  ret i8* %deref
}

declare i8** @JudyLIns(i8**, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLGet(i8*, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLFirst(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLNext(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)


