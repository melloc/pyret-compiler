;; This file provides examples of using libgmp in LLVM IR.
;;
;;

target triple = "x86_64-pc-linux-gnu"

;; we want malloc
declare noalias i8* @malloc(i64) nounwind

;; mpz_t struct
%struct.__mpz_struct = type { i32, i32, i64* }

declare void @__gmpz_init(%struct.__mpz_struct*)

declare i32 @__gmpz_set_str(%struct.__mpz_struct*, i8*, i32)

declare void @__gmpz_mul(%struct.__mpz_struct*, %struct.__mpz_struct*, %struct.__mpz_struct*)

declare void @__gmpz_clear(%struct.__mpz_struct*)

;; Pyret Number
;; Type: 0
;; Variants:
;;  - Rational (0)
;;  - Float (1)
%struct.pyret-number = type { i32, i8, %struct.__mpz_struct* }

define %struct.pyret-number* @initialize-integer(i8* %str) {
    ;; Create GMP Rational Number
    %sizeof.__mpz_struct-tmp = getelementptr %struct.__mpz_struct* null, i64 1
    %sizeof.__mpz_struct = ptrtoint %struct.__mpz_struct* %sizeof.__mpz_struct-tmp to i64
    %gmp-num-mem = call noalias i8* @malloc(i64 %sizeof.pyret-number) nounwind
    %gmp-num-mem-pointer = bitcast i8* %gmp-num-mem to %struct.__mpz_struct*
    call void @__gmpz_init(%struct.__mpz_struct* %gmp-num-mem-pointer)
    call i32 @__gmpz_set_str(%struct.__mpz_struct* %gmp-num-mem-pointer, i8* %str, i32 10)

    ;; Set up Pyret Rational Number
    %sizeof.pyret-number-tmp = getelementptr %struct.pyret-number* null, i64 1
    %sizeof.pyret-number = ptrtoint %struct.pyret-number* %sizeof.pyret-number-tmp to i64
    %py-num-mem = call noalias i8* @malloc(i64 %sizeof.pyret-number) nounwind
    %py-num-mem-pointer = bitcast i8* %py-num-mem to %struct.pyret-number*

    ;; Set type field to Pyret Number
    %type-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 0
    store i32 0, i32* %type-field

    ;; Set variant field to Pyret Rational Number
    %variant-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 1
    store i8 0, i8* %variant-field

    ;; Set field to GMP number
    %mpz-field = getelementptr %struct.pyret-number* %py-num-mem-pointer, i64 0, i32 2
    store %struct.__mpz_struct* %gmp-num-mem-pointer, %struct.__mpz_struct** %mpz-field
    ret %struct.pyret-number* %py-num-mem-pointer
}

define i32 @main() {
    ret i32 0
}
