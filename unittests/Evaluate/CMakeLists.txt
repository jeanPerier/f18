add_library(FortranEvaluateTesting
  testing.cpp
  fp-testing.cpp
)

add_flang_test(leading-zero-bit-count Leadz
  FortranEvaluateTesting
  LLVMSupport
)

add_flang_test(bit-population-count PopPar
  FortranEvaluateTesting
)

add_flang_test(uint128 UINT128
  FortranEvaluateTesting
)

add_flang_test(expression Expression
  FortranEvaluateTesting
  FortranEvaluate
  FortranSemantics
  FortranParser
  LLVMSupport
)

add_flang_test(integer Integer
  FortranEvaluateTesting
  FortranEvaluate
  FortranSemantics
  LLVMSupport
)

add_flang_test(intrinsics Intrinsics
  FortranEvaluateTesting
  FortranEvaluate
  FortranDecimal
  FortranSemantics
  FortranParser
  FortranRuntime
  LLVMSupport
)

add_flang_test(logical Logical
  FortranEvaluateTesting
  FortranEvaluate
  FortranSemantics
  LLVMSupport
)

# GCC -fno-exceptions breaks the fenv.h interfaces needed to capture
# IEEE exception flags (different use of the word "exception")
# in the actual hardware floating-point status register, so ensure that
# C++ exceptions are enabled for this test.
set_source_files_properties(real.cpp PROPERTIES COMPILE_FLAGS -fexceptions)

add_flang_test(real Real
  FortranEvaluateTesting
  FortranEvaluate
  FortranDecimal
  FortranSemantics
  LLVMSupport
)

add_flang_test(reshape Reshape
  FortranEvaluateTesting
  FortranSemantics
  FortranEvaluate
  FortranRuntime
  LLVMSupport
)

add_flang_test(ISO-Fortran-binding ISO-binding
  FortranEvaluateTesting
  FortranEvaluate
  FortranSemantics
  FortranRuntime
  LLVMSupport
)

add_flang_test(folding Folding
  FortranEvaluateTesting
  FortranEvaluate
  FortranSemantics
  LLVMSupport
)

add_test(Expression expression-test)
add_test(Integer integer-test)
add_test(Intrinsics intrinsics-test)
add_test(Logical logical-test)
add_test(Real real-test)
add_test(RESHAPE reshape-test)
add_test(ISO-binding ISO-Fortran-binding-test)
add_test(folding folding-test)
