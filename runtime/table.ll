target triple = "x86_64-unknown-linux-gnu"

%struct.A = type { i32, i32 }
%struct.J_UDY_ERROR_STRUCT = type { i32, i32, [4 x i64] }

define void @table-insert(i8** %PJLArray, i64 %key, i64 %to-insert) nounwind {
  %PValue = alloca i64*                                     ;; Allocate space for PValue
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %ins-ret = call i8** @JudyLIns(i8** %PJLArray, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error) ;; Make a call to JudyLIns
  %ins-ret-tmp = bitcast i8** %ins-ret to i8*               ;; Cast result from i8** to i8*
  %ins-ret-loc = bitcast i8* %ins-ret-tmp to i64*           ;; Case result from i8* to i64*
  store i64 %to-insert, i64* %ins-ret-loc                   ;; Store pointer to a there. 
  ;; At some point, we should do some checking for errors here.
  ret void
}

define i64 @table-lookup(i8** %PJLArray, i64 %key) nounwind {
  %deref-array = load i8** %PJLArray         
  %J_Error = alloca %struct.J_UDY_ERROR_STRUCT              ;; Allocate space for J_Error, which contains Judy errors
  %lookup-ret = call i8** @JudyLGet(i8* %deref-array, i64 %key, %struct.J_UDY_ERROR_STRUCT* %J_Error)
  ;; %lookup-ret-tmp = bitcast i8** %ins-lookup to i8*         ;; Cast result from i8** to i8*
  %lookup-ret-tmp = bitcast i8** %lookup-ret to i8*         ;; Cast result from i8** to i8*
  %lookup-ret-loc = bitcast i8* %lookup-ret-tmp to i64*     ;; Case result from i8* to i64*
  %deref = load i64* %lookup-ret-loc                        ;; Dereference that.
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
  %res = call i64 @table-lookup(i8** %PJLArray, i64 5)
  ret i64 %res 
}

declare i8** @JudyLIns(i8**, i64, %struct.J_UDY_ERROR_STRUCT*)
declare i8** @JudyLGet(i8*, i64, %struct.J_UDY_ERROR_STRUCT*)
