
%struct.J_UDY_ERROR_STRUCT = type { i32, i32, [4 x i64] }

define i8* @table-copy(i8* %PJLArray) nounwind {
  %PValueA = alloca i8**
  %dst = alloca i8*
  store i8* null, i8** %dst
  %i = alloca i64
  store i64 0, i64* %i
  %first-ret = call i8** @JudyLFirst(i8* %PJLArray, i64* %i, %struct.J_UDY_ERROR_STRUCT* null)
  store i8** %first-ret, i8*** %PValueA
  br label %comparison

comparison:                                       ; preds = %copy-entry, %0
  %comparison-pval = load i8*** %PValueA
  %is-null = icmp ne i8** %comparison-pval, null
  br i1 %is-null, label %copy-entry, label %exit

copy-entry:                                       ; preds = %comparison
  %deref-i = load i64* %i
  %ins-ret = call i8** @JudyLIns(i8** %dst, i64 %deref-i, %struct.J_UDY_ERROR_STRUCT* null)
  %copy-entry-pval = load i8*** %PValueA
  %copy-entry-val = load i8** %copy-entry-pval
  store i8* %copy-entry-val, i8** %ins-ret
  %next-ret = call i8** @JudyLNext(i8* %PJLArray, i64* %i, %struct.J_UDY_ERROR_STRUCT* null)
  store i8** %next-ret, i8*** %PValueA
  br label %comparison

exit:                                      ; preds = %comparison
  %ret = load i8** %dst
  ret i8* %ret
}


define void @table-insert(i8** %PJLArray, i64 %key, i8* %to-insert) nounwind {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %ins-ret = call i8** @JudyLIns(i8** %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error) ;; Make a call to JudyLIns
  store i8* %to-insert, i8** %ins-ret                       ;; Store pointer to a there. 
  ;; At some point, we should do some checking for errors here.
  ret void
}

define i8* @table-lookup(i8* %PJLArray, i64 %key) nounwind {
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %lookup-ret = call i8** @JudyLGet(i8* %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  %deref = load i8** %lookup-ret                            ;; Dereference that.
  ret i8* %deref
}

declare i8** @JudyLIns(i8**, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLGet(i8*, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLFirst(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLNext(i8*, i64*, %struct.J_UDY_ERROR_STRUCT*)
