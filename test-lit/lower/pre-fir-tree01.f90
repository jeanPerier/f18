! RUN: %f18 -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test structure of the Pre-FIR tree

! CHECK: PFT root node:[[#%u, ROOT:]]
! CHECK: [[#%u, FOO:]]{{.*}}Subroutine foo{{.*}}parent:[[#ROOT]]
subroutine foo()
  ! CHECK: [[#%u, DO1:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#FOO]]
  ! CHECK: NonLabelDoStmt{{.*}}parent:[[#DO1]]
  do i=1,5
    ! CHECK: PrintStmt{{.*}}parent:[[#DO1]]
    print *, "hey"
    ! CHECK: [[#%u, DO2:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#DO1]]
    do j=1,5
      ! CHECK: PrintStmt{{.*}}parent:[[#DO2]]
      print *, "hello", i, j
    ! CHECK: EndDoStmt{{.*}}parent:[[#DO2]]
    end do
  ! CHECK: EndDoStmt{{.*}}parent:[[#DO1]]
  end do
! CHECK: EndSubroutine
end subroutine

! CHECK: BlockData{{.*}}parent:[[#ROOT]]
block data
  integer, parameter :: n = 100
  integer, dimension(n) :: a, b, c
  common /arrays/ a, b, c
end

! CHECK: [[#%u, TEST_MOD:]]{{.*}}ModuleLike{{.*}}parent:[[#ROOT]]
module test_mod
interface
  ! check specification parts are not part of the PFT.
  ! CHECK-NOT: node
  module subroutine dump()
  end subroutine
end interface
 integer :: xdim
 real, allocatable :: pressure(:)
contains
  ! CHECK: [[#%u, M_FOO:]]{{.*}}Subroutine foo{{.*}}parent:[[#TEST_MOD]]
  subroutine foo()
    contains
    ! CHECK: [[#%u, SUBFOO:]]{{.*}}Subroutine subfoo{{.*}}parent:[[#M_FOO]]
    subroutine subfoo()
    end subroutine
    ! CHECK: [[#%u, SUBFOO2:]]{{.*}}Function subfoo2{{.*}}parent:[[#M_FOO]]
    function subfoo2()
    end function
  end subroutine

  ! CHECK: [[#%u, M_FOO2:]]{{.*}}Function foo2{{.*}}parent:[[#TEST_MOD]]
  function foo2(i, j)
    integer i, j, foo2
    ! CHECK: AssignmentStmt{{.*}}parent:[[#M_FOO2]]
    foo2 = i + j
    contains
    ! CHECK: [[#%u, SUBFOO:]]{{.*}}Subroutine subfoo{{.*}}parent:[[#M_FOO2]]
    subroutine subfoo()
    end subroutine
  end function
end module

! CHECK: [[#%u, SUB_MOD:]]{{.*}}ModuleLike{{.*}}parent:[[#ROOT]]
submodule (test_mod) test_mod_impl
contains
  ! CHECK: [[#%u, SUBM_FOO:]]{{.*}}Subroutine foo{{.*}}parent:[[#SUB_MOD]]
  subroutine foo()
    contains
    ! CHECK: [[#%u, SUBFOO:]]{{.*}}Subroutine subfoo{{.*}}parent:[[#SUBM_FOO]]
    subroutine subfoo()
    end subroutine
    ! CHECK: [[#%u, SUBFOO2:]]{{.*}}Function subfoo2{{.*}}parent:[[#SUBM_FOO]]
    function subfoo2()
    end function
  end subroutine
  ! CHECK: [[#%u, MP_DUMP:]]{{.*}}MpSubprogram dump{{.*}}parent:[[#SUB_MOD]]
  module procedure dump
    ! CHECK: FormatStmt{{.*}}parent:[[#MP_DUMP]]
11  format (2E16.4, I6)
    ! CHECK: [[#%u, IF1:]]{{.*}}<<IfConstruct>>{{.*}}parent:[[#MP_DUMP]]
    ! CHECK: IfThenStmt{{.*}}parent:[[#IF1]]
    if (xdim > 100) then
      ! CHECK: PrintStmt{{.*}}parent:[[#IF1]]
      print *, "test: ", xdim
    ! CHECK: ElseStmt{{.*}}parent:[[#IF1]]
    else
      ! CHECK: WriteStmt{{.*}}parent:[[#IF1]]
      write (*, 11) "test: ", xdim, pressure
    ! CHECK: EndIfStmt{{.*}}parent:[[#IF1]]
    end if
  end procedure
end submodule

! CHECK: BlockData{{.*}}parent:[[#ROOT]]
block data
 integer i, j, k
 common /indexes/ i, j, k
end

! CHECK: [[#%u, BAR:]]{{.*}}Function bar{{.*}}parent:[[#ROOT]]
function bar()
end function

! CHECK: [[#%u, PROG:]]{{.*}}Program <anonymous>{{.*}}parent:[[#ROOT]]
  ! check specification parts are not part of the PFT.
  ! CHECK-NOT: node
  use test_mod
  real, allocatable :: x(:)
  ! CHECK: AllocateStmt{{.*}}parent:[[#PROG]]
  allocate(x(foo2(10, 30)))
end
