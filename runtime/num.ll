;; This file provides the basic runtime environment for Pyret.
;; Currently, the only functions provided are those related to doing 
;; arithmetic in Pyret.
;;
;; When building this file, you should link against libgmp. For example:
;;
;; clang -Xlinker /usr/lib64/libgmp.so -o my-prog my-prog.ll
;;

;; mpz_t (integer) struct
%struct.__mpz_struct = type { i32, i32, i64* }
;; mpq_t (rational) struct
%struct.__mpq_struct = type { %struct.__mpz_struct, %struct.__mpz_struct }
;; mpf_t (float) struct
%struct.__mpf_struct = type { i32, i32, i64, i64* }

declare void @__gmpf_init(%struct.__mpf_struct*)

declare i32 @__gmpf_set_str(%struct.__mpf_struct*, i8*, i32)

declare void @__gmpf_mul(%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)

declare void @__gmpf_add(%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)

declare void @__gmpf_sub(%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)

declare void @__gmpf_div(%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)

declare i32 @__gmpf_cmp(%struct.__mpf_struct*, %struct.__mpf_struct*)

declare void @__gmpf_clear(%struct.__mpf_struct*)

declare i32 @__gmp_printf(i8*, ...)

;; Pyret Number
;; Type: 0
@pyret-number-type = private constant i32 0
;; Variants:
;;  - Rational (0)
@pyret-number-rational = private constant i32 0
;;  - Float (1)
@pyret-number-float = private constant i32 1
;; Field: 0
@pyret-number-field = private constant i64 0

define %struct.__mpf_struct* @get-float(%struct.pyret-value %number) alwaysinline {
    %table = extractvalue %struct.pyret-value %number, 2
    %key = load i64* @pyret-number-field
    %gmp-num-ptr = call i8* @table-lookup(i8* %table, i64 %key)
    %gmp-num = bitcast i8* %gmp-num-ptr to %struct.__mpf_struct*
    ret %struct.__mpf_struct* %gmp-num
}

define %struct.pyret-value @allocate-float() {
    ;; Create GMP Float Number
    %gmp-num-mem = call noalias i8* @malloc(i64 32) nounwind
    %gmp-num-mem-pointer = bitcast i8* %gmp-num-mem to %struct.__mpf_struct*
    call void @__gmpf_init(%struct.__mpf_struct* %gmp-num-mem-pointer)

    ;; Set up Pyret Number by setting type field
    %type-value = load i32* @pyret-number-type
    %py-num-tmp-a = insertvalue %struct.pyret-value undef, i32 %type-value, 0

    ;; Set variant field to Pyret Float Number
    %variant-value = load i32* @pyret-number-float
    %py-num-tmp-b = insertvalue %struct.pyret-value %py-num-tmp-a, i32 %variant-value, 1

    ;; Allocate space for table and set number field to GMP number
    %table-ptr = alloca i8*
    store i8* null, i8** %table-ptr
    %gmp-bitcast = bitcast %struct.__mpf_struct* %gmp-num-mem-pointer to i8*
    %key = load i64* @pyret-number-field
    call i8* @table-insert(i8** %table-ptr, i64 %key, i8* %gmp-bitcast)
    %table = load i8** %table-ptr

    ;; Set table field to new Judy Array, and create final Pyret Number
    %py-num = insertvalue %struct.pyret-value %py-num-tmp-b, i8* %table, 2
    ret %struct.pyret-value %py-num
}

define %struct.pyret-value @initialize-integer(i8* %str) {
    %pyret-float = call %struct.pyret-value @allocate-float()
    %gmp-float = call %struct.__mpf_struct* @get-float(%struct.pyret-value %pyret-float)
    call i32 @__gmpf_set_str(%struct.__mpf_struct* %gmp-float, i8* %str, i32 10)
    ret %struct.pyret-value %pyret-float
}


define %struct.pyret-value @float-arithmetic-method(%struct.pyret-value %a, %struct.pyret-value %b, void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* %f) {
    ;; Allocate the result
    %c-pyret-value = call %struct.pyret-value @allocate-float()
    %c-mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %c-pyret-value)

    ;; We assume that a is a Float Number
    ;; Currently we only operate on b if it is also a Float Number.
    ;; At some point it would be nice to convert it to a Float Number.
    ;; b should be a Number
    %b-type-value = extractvalue %struct.pyret-value %b, 0
    %b-type-is-zero = icmp eq i32 %b-type-value, 0
    br i1 %b-type-is-zero, label %compare-variant, label %exit

    compare-variant:
    ;; Now we want to check b's variant
    %b-variant-value = extractvalue %struct.pyret-value %b, 1
    %b-variant-is-one = icmp eq i32 %b-variant-value, 1
    br i1 %b-variant-is-one, label %case-float, label %case-float-else

    case-float:
    %a-mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %a)
    %b-mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %b)
    call void %f(%struct.__mpf_struct* %c-mpf, %struct.__mpf_struct* %a-mpf, %struct.__mpf_struct* %b-mpf)
    ;; Done. Now exit.
    br label %exit

    case-float-else:
    ;; Currently we only support Floats, so exit.
    br label %exit

    exit:
    ret %struct.pyret-value %c-pyret-value
}

define %struct.pyret-value @rational-plus-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %result = call %struct.pyret-value @float-arithmetic-method(
            %struct.pyret-value %a, 
            %struct.pyret-value %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_add)
    ret %struct.pyret-value %result
}

define %struct.pyret-value @rational-minus-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %result = call %struct.pyret-value @float-arithmetic-method(
            %struct.pyret-value %a, 
            %struct.pyret-value %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_sub)
    ret %struct.pyret-value %result
}

define %struct.pyret-value @rational-divide-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %result = call %struct.pyret-value @float-arithmetic-method(
            %struct.pyret-value %a, 
            %struct.pyret-value %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_div)
    ret %struct.pyret-value %result
}

define %struct.pyret-value @rational-times-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %result = call %struct.pyret-value @float-arithmetic-method(
            %struct.pyret-value %a, 
            %struct.pyret-value %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_mul)
    ret %struct.pyret-value %result
}


define i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    ;; We assume that a is a Float Number
    ;; Currently we only operate on b if it is also a Float Number.
    ;; At some point it would be nice to convert it to a Float Number.
    ;; b should be a Number
    %b-type-value = extractvalue %struct.pyret-value %b, 0
    %b-type-is-zero = icmp eq i32 %b-type-value, 0
    br i1 %b-type-is-zero, label %compare-variant, label %exit

    compare-variant:
    ;; Now we want to check b's variant
    %b-variant-value = extractvalue %struct.pyret-value %b, 1
    %b-variant-is-one = icmp eq i32 %b-variant-value, 1
    br i1 %b-variant-is-one, label %case-float, label %case-float-else

    case-float:
    %a-mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %a)
    %b-mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %b)
    %ret = call i32 @__gmpf_cmp(%struct.__mpf_struct* %a-mpf, %struct.__mpf_struct* %b-mpf)
    ;; Done. Now exit.
    ret i32 %ret

    case-float-else:
    ;; Currently we only support Floats, so exit.
    br label %exit

    exit:
    ret i32 -1
}

define %struct.pyret-value @rational-lt-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp slt i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

define %struct.pyret-value @rational-lte-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp sle i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

define %struct.pyret-value @rational-gt-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp sgt i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

define %struct.pyret-value @rational-gte-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp sge i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

define %struct.pyret-value @rational-ne-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp ne i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

define %struct.pyret-value @rational-equals-method(%struct.pyret-value %a, %struct.pyret-value %b) {
    %cmp = call i32 @float-comparison-method(%struct.pyret-value %a, %struct.pyret-value %b)
    %is-equal = icmp eq i32 %cmp, 0
    br i1 %is-equal, label %true-branch, label %false-branch
true-branch:
    %true-value = load %struct.pyret-value* @true
    ret %struct.pyret-value %true-value
false-branch:
    %false-value = load %struct.pyret-value* @false
    ret %struct.pyret-value %false-value
}

@math.rational-print-string = private unnamed_addr constant [5 x i8] c"%Qd\0A\00"
@math.float-print-string = private unnamed_addr constant [5 x i8] c"%Ff\0A\00"

define void @print-pyret-value(%struct.pyret-value %number) {
    ;; Get the field
    %mpf = call %struct.__mpf_struct* @get-float(%struct.pyret-value %number)

    ;; Get the variant tag
    %variant-value = extractvalue %struct.pyret-value %number, 1
    %float-tag = load i32* @pyret-number-float
    %is-float = icmp eq i32 %variant-value, %float-tag
    br i1 %is-float, label %case-float, label %case-float-else

    case-float:
    %printed = call i32 (i8*, ...)* @__gmp_printf(i8* getelementptr inbounds ([5 x i8]* @math.float-print-string, i32 0, i32 0), %struct.__mpf_struct* %mpf)

    ;; Done. Go to exit.
    br label %exit

    case-float-else:
    ;; Nothing else exists yet
    br label %exit

    exit:
    ret void
}
