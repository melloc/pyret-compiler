;; This file provides examples of using libgmp in LLVM IR.
;;
;;

target triple = "x86_64-pc-linux-gnu"

;; we want malloc
declare noalias i8* @malloc(i64) nounwind

;; mpz_t (integer) struct
%struct.__mpz_struct = type { i32, i32, i64* }
;; mpq_t (rational) struct
%struct.__mpq_struct = type { %struct.__mpz_struct, %struct.__mpz_struct }

declare void @__gmpq_init(%struct.__mpq_struct*)

declare i32 @__gmpq_set_str(%struct.__mpq_struct*, i8*, i32)

declare void @__gmpq_mul(%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)

declare void @__gmpq_add(%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)

declare void @__gmpq_sub(%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)

declare void @__gmpq_div(%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)

declare void @__gmpq_clear(%struct.__mpq_struct*)

declare i32 @__gmp_printf(i8*, ...)

;; Pyret Number
;; Type: 0
;; Variants:
;;  - Rational (0)
;;  - Float (1)
%struct.pyret-number = type { i32, i8, %struct.__mpq_struct* }

define %struct.pyret-number* @allocate-rational() {
    ;; Create GMP Rational Number
    %sizeof.__mpq_struct-tmp = getelementptr %struct.__mpq_struct* null, i64 1
    %sizeof.__mpq_struct = ptrtoint %struct.__mpq_struct* %sizeof.__mpq_struct-tmp to i64
    ;; TODO: Fix calculating size
    %gmp-num-mem = call noalias i8* @malloc(i64 32) nounwind
    %gmp-num-mem-pointer = bitcast i8* %gmp-num-mem to %struct.__mpq_struct*
    call void @__gmpq_init(%struct.__mpq_struct* %gmp-num-mem-pointer)

    ;; Set up Pyret Rational Number
    %sizeof.pyret-number-tmp = getelementptr %struct.pyret-number* null, i64 1
    %sizeof.pyret-number = ptrtoint %struct.pyret-number* %sizeof.pyret-number-tmp to i64
    ;; TODO: Fix calculating size
    %py-num-mem = call noalias i8* @malloc(i64 13) nounwind
    %py-num-mem-pointer = bitcast i8* %py-num-mem to %struct.pyret-number*

    ;; Set type field to Pyret Number
    %type-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 0
    store i32 0, i32* %type-field

    ;; Set variant field to Pyret Rational Number
    %variant-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 1
    store i8 0, i8* %variant-field

    ;; Set field to GMP number
    %mpq-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 2
    store %struct.__mpq_struct* %gmp-num-mem-pointer, %struct.__mpq_struct** %mpq-field
    ret %struct.pyret-number* %py-num-mem-pointer
}

define %struct.pyret-number* @initialize-integer(i8* %str) {
    %pyret-rational = call %struct.pyret-number* @allocate-rational()
    %gmp-rational-field   = getelementptr %struct.pyret-number* %pyret-rational, i64 0, i32 2
    %gmp-rational = load %struct.__mpq_struct** %gmp-rational-field
    call i32 @__gmpq_set_str(%struct.__mpq_struct* %gmp-rational, i8* %str, i32 10)
    ret %struct.pyret-number* %pyret-rational
}


define %struct.pyret-number* @rational-arithmetic-method(%struct.pyret-number* %a, %struct.pyret-number* %b, void (%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)* %f) {
    ;; Allocate the result
    %c-pyret-number = call %struct.pyret-number* @allocate-rational()
    %c-mpq-field    = getelementptr %struct.pyret-number* %c-pyret-number, i64 0, i32 2
    %c-mpq = load %struct.__mpq_struct** %c-mpq-field

    ;; We assume that a is a Rational Number
    ;; Currently we only operate on b if it is also a Rational Number.
    ;; At some point it would be nice to convert it to a Rational Number.
    ;; b should be a Number
    %b-type-field = getelementptr %struct.pyret-number* %b, i64 0, i32 0
    %b-type-value = load i32* %b-type-field
    %b-type-is-zero = icmp eq i32 %b-type-value, 0
    br i1 %b-type-is-zero, label %compare-variant, label %exit

    compare-variant:
    ;; Now we want to check b's variant
    %b-variant-field = getelementptr %struct.pyret-number* %b, i64 0, i32 1
    %b-variant-value = load i8* %b-variant-field
    %b-variant-is-zero = icmp eq i8 %b-variant-value, 0
    br i1 %b-variant-is-zero, label %case-rational, label %case-rational-else

    case-rational:
    %a-mpq-field = getelementptr %struct.pyret-number* %a, i64 0, i32 2
    %a-mpq = load %struct.__mpq_struct** %a-mpq-field
    %b-mpq-field = getelementptr %struct.pyret-number* %b, i64 0, i32 2
    %b-mpq = load %struct.__mpq_struct** %b-mpq-field
    call void %f(%struct.__mpq_struct* %c-mpq, %struct.__mpq_struct* %a-mpq, %struct.__mpq_struct* %b-mpq)
    ;; Done. Now exit.
    br label %exit

    case-rational-else:
    ;; Currently we only support Rationals, so exit.
    br label %exit

    exit:
    ret %struct.pyret-number* %c-pyret-number
}

define %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @rational-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)* @__gmpq_add)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-minus-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @rational-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)* @__gmpq_sub)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-divide-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @rational-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)* @__gmpq_div)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-times-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @rational-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpq_struct*, %struct.__mpq_struct*, %struct.__mpq_struct*)* @__gmpq_mul)
    ret %struct.pyret-number* %result
}

@math.rational-print-string = private unnamed_addr constant [5 x i8] c"%Zd\0A\00"

define void @print-pyret-number(%struct.pyret-number* %number) {
    ;; Get the field
    %mpq-field    = getelementptr %struct.pyret-number* %number, i64 0, i32 2
    %mpq = load %struct.__mpq_struct** %mpq-field

    ;; Get the variant tag
    %variant-field = getelementptr %struct.pyret-number* %number, i64 0, i32 1
    %variant-value = load i8* %variant-field
    %variant-is-zero = icmp eq i8 %variant-value, 0
    br i1 %variant-is-zero, label %case-rational, label %case-rational-else

    case-rational:
    %printed = call i32 (i8*, ...)* @__gmp_printf(i8* getelementptr inbounds ([5 x i8]* @math.rational-print-string, i32 0, i32 0), %struct.__mpq_struct* %mpq)

    ;; Done. Go to exit.
    br label %exit

    case-rational-else:
    ;; Nothing else exists yet
    br label %exit

    exit:
    ret void
}

@num1 = private unnamed_addr constant [17 x i8] c"7612058254738945\00"
@num2 = private unnamed_addr constant [17 x i8] c"9263591128439081\00"

define i32 @main() {
    %a = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([17 x i8]* @num1, i32 0, i32 0))
    %b = call %struct.pyret-number* @initialize-integer(i8* getelementptr inbounds ([17 x i8]* @num2, i32 0, i32 0))

    ;; Example addition
    %a-add-b = call %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %a, %struct.pyret-number* %b)
    %b-add-a = call %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %b, %struct.pyret-number* %a)

    ;; Example subtraction
    %a-sub-b = call %struct.pyret-number* @rational-minus-method(%struct.pyret-number* %a, %struct.pyret-number* %b)
    %b-sub-a = call %struct.pyret-number* @rational-minus-method(%struct.pyret-number* %b, %struct.pyret-number* %a)

    ;; Example multiplication
    %a-times-b = call %struct.pyret-number* @rational-times-method(%struct.pyret-number* %a, %struct.pyret-number* %b)
    %b-times-a = call %struct.pyret-number* @rational-times-method(%struct.pyret-number* %b, %struct.pyret-number* %a)

    ;; Example division
    %a-over-b = call %struct.pyret-number* @rational-divide-method(%struct.pyret-number* %a, %struct.pyret-number* %b)
    %b-over-a = call %struct.pyret-number* @rational-divide-method(%struct.pyret-number* %b, %struct.pyret-number* %a)

    call void @print-pyret-number(%struct.pyret-number* %a-add-b)
    call void @print-pyret-number(%struct.pyret-number* %b-add-a)
    call void @print-pyret-number(%struct.pyret-number* %a-sub-b)
    call void @print-pyret-number(%struct.pyret-number* %b-sub-a)
    call void @print-pyret-number(%struct.pyret-number* %a-times-b)
    call void @print-pyret-number(%struct.pyret-number* %b-times-a)
    call void @print-pyret-number(%struct.pyret-number* %a-over-b)
    call void @print-pyret-number(%struct.pyret-number* %b-over-a)

    ret i32 0
}
