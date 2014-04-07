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

declare void @__gmpf_clear(%struct.__mpf_struct*)

declare i32 @__gmp_printf(i8*, ...)

;; Pyret Number
;; Type: 0
;; Variants:
;;  - Rational (0)
;;  - Float (1)
%struct.pyret-number = type { i32, i8, %struct.__mpf_struct* }

define %struct.pyret-number* @allocate-float() {
    ;; Create GMP Rational Number
    %sizeof.__mpf_struct-tmp = getelementptr %struct.__mpf_struct* null, i64 1
    %sizeof.__mpf_struct = ptrtoint %struct.__mpf_struct* %sizeof.__mpf_struct-tmp to i64
    ;; TODO: Fix calculating size
    %gmp-num-mem = call noalias i8* @malloc(i64 32) nounwind
    %gmp-num-mem-pointer = bitcast i8* %gmp-num-mem to %struct.__mpf_struct*
    call void @__gmpf_init(%struct.__mpf_struct* %gmp-num-mem-pointer)

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
    %mpf-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 2
    store %struct.__mpf_struct* %gmp-num-mem-pointer, %struct.__mpf_struct** %mpf-field
    ret %struct.pyret-number* %py-num-mem-pointer
}

define %struct.pyret-number* @initialize-integer(i8* %str) {
    %pyret-float = call %struct.pyret-number* @allocate-float()
    %gmp-float-field   = getelementptr %struct.pyret-number* %pyret-float, i64 0, i32 2
    %gmp-float = load %struct.__mpf_struct** %gmp-float-field
    call i32 @__gmpf_set_str(%struct.__mpf_struct* %gmp-float, i8* %str, i32 10)
    ret %struct.pyret-number* %pyret-float
}


define %struct.pyret-number* @float-arithmetic-method(%struct.pyret-number* %a, %struct.pyret-number* %b, void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* %f) {
    ;; Allocate the result
    %c-pyret-number = call %struct.pyret-number* @allocate-float()
    %c-mpf-field    = getelementptr %struct.pyret-number* %c-pyret-number, i64 0, i32 2
    %c-mpf = load %struct.__mpf_struct** %c-mpf-field

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
    br i1 %b-variant-is-zero, label %case-float, label %case-float-else

    case-float:
    %a-mpf-field = getelementptr %struct.pyret-number* %a, i64 0, i32 2
    %a-mpf = load %struct.__mpf_struct** %a-mpf-field
    %b-mpf-field = getelementptr %struct.pyret-number* %b, i64 0, i32 2
    %b-mpf = load %struct.__mpf_struct** %b-mpf-field
    call void %f(%struct.__mpf_struct* %c-mpf, %struct.__mpf_struct* %a-mpf, %struct.__mpf_struct* %b-mpf)
    ;; Done. Now exit.
    br label %exit

    case-float-else:
    ;; Currently we only support Rationals, so exit.
    br label %exit

    exit:
    ret %struct.pyret-number* %c-pyret-number
}

define %struct.pyret-number* @rational-plus-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @float-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_add)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-minus-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @float-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_sub)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-divide-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @float-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_div)
    ret %struct.pyret-number* %result
}

define %struct.pyret-number* @rational-times-method(%struct.pyret-number* %a, %struct.pyret-number* %b) {
    %result = call %struct.pyret-number* @float-arithmetic-method(
            %struct.pyret-number* %a, 
            %struct.pyret-number* %b, 
            void (%struct.__mpf_struct*, %struct.__mpf_struct*, %struct.__mpf_struct*)* @__gmpf_mul)
    ret %struct.pyret-number* %result
}

@math.rational-print-string = private unnamed_addr constant [5 x i8] c"%Qd\0A\00"
@math.float-print-string = private unnamed_addr constant [5 x i8] c"%Ff\0A\00"

define void @print-pyret-number(%struct.pyret-number* %number) {
    ;; Get the field
    %mpf-field    = getelementptr %struct.pyret-number* %number, i64 0, i32 2
    %mpf = load %struct.__mpf_struct** %mpf-field

    ;; Get the variant tag
    %variant-field = getelementptr %struct.pyret-number* %number, i64 0, i32 1
    %variant-value = load i8* %variant-field
    %variant-is-zero = icmp eq i8 %variant-value, 0
    br i1 %variant-is-zero, label %case-float, label %case-float-else

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

