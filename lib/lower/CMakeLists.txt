set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-error -Wno-unused-parameter")

add_library(FortranLower
  PFTBuilder.cpp
)

target_link_libraries(FortranLower
  MLIRAffineOps
  MLIRLLVMIR
  MLIRLoopToStandard
  MLIRStandardOps
)

install (TARGETS FortranLower
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin
)
