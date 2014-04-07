target triple = "x86_64-unknown-linux-gnu"

%struct.J_UDY_ERROR_STRUCT = type { i32, i32, [4 x i64] }

define i8* @table-copy(i8** %PJLArray) nounwind {
  %deref-array = load i8** %PJLArray         
  %PValueA = alloca i64*
  %dst = alloca i8*
  store i8* null, i8** %dst
  %i = alloca i64
  store i64 0, i64* %i
  %first-ret = call i64* @JudyLFirst(i8* %deref-array, i64* %i, %struct.J_UDY_ERROR_STRUCT* null)
  store i64* %first-ret, i64** %PValueA
  br label %comparison

comparison:                                       ; preds = %copy-entry, %0
  %comparison-pval = load i64** %PValueA
  %is-null = icmp ne i64* %comparison-pval, null
  br i1 %is-null, label %copy-entry, label %exit

copy-entry:                                       ; preds = %comparison
  %deref-i = load i64* %i
  %ins-ret = call i64* @JudyLIns(i8** %dst, i64 %deref-i, %struct.J_UDY_ERROR_STRUCT* null)
  %copy-entry-pval = load i64** %PValueA
  %copy-entry-val = load i64* %copy-entry-pval
  store i64 %copy-entry-val, i64* %ins-ret
  %next-ret = call i64* @JudyLNext(i8* %deref-array, i64* %i, %struct.J_UDY_ERROR_STRUCT* null)
  store i64* %next-ret, i64** %PValueA
  br label %comparison

exit:                                      ; preds = %comparison
  %ret = load i8** %dst
  ret i8* %ret
}


define void @table-insert(i8** %PJLArray, i64 %key, i64 %to-insert) nounwind {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %ins-ret = call i64* @JudyLIns(i8** %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error) ;; Make a call to JudyLIns
  store i64 %to-insert, i64* %ins-ret                       ;; Store pointer to a there. 
  ;; At some point, we should do some checking for errors here.
  ret void
}

define i64 @table-lookup(i8** %PJLArray, i64 %key) nounwind {
  %deref-array = load i8** %PJLArray         
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %lookup-ret = call i64* @JudyLGet(i8* %deref-array, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  %deref = load i64* %lookup-ret                            ;; Dereference that.
  ret i64 %deref
}

define i64 @main(i32 %argc, i8** %argv) nounwind {
  %1 = alloca i32         
  %2 = alloca i32         
  %3 = alloca i8**         
  store i32 0, i32* %1
  store i32 %argc, i32* %2         
  store i8** %argv, i8*** %3         
  %PJLArray = alloca i8*                                    ;; Allocate space for PJLArray
  store i8* null, i8** %PJLArray         
  call void @table-insert(i8** %PJLArray, i64 5, i64 10)
  %new-table = alloca i8*
  %copy-ret = call i8* @copy(i8** %PJLArray)
  store i8* %copy-ret, i8** %new-table
  %res = call i64 @table-lookup(i8** %new-table, i64 5)
  ret i64 %res 
}

declare i64* @JudyLIns(i8**, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i64* @JudyLGet(i8*, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i64* @JudyLFirst(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)
declare i64* @JudyLNext(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)
