! RUN: %f18 -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test structure of the Pre-FIR tree

! CHECK: PFT root node:0x[[#%x, ROOT:]]
! CHECK: Subroutine foo{{.*}} node:0x[[#%x, FOO:]] parent:0x[[#ROOT]]
subroutine foo()
  ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO1:]] parent:0x[[#FOO]]
  ! CHECK: NonLabelDoStmt{{.*}} parent:0x[[#DO1]]
  do i=1,5
    ! CHECK: PrintStmt{{.*}} parent:0x[[#DO1]]
    print *, "hey"
    ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO2:]] parent:0x[[#DO1]]
    do j=1,5
      ! CHECK: PrintStmt{{.*}} parent:0x[[#DO2]]
      print *, "hello", i, j
    ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO2]]
    end do
  ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO1]]
  end do
! CHECK: EndSubroutine
end subroutine

! CHECK: BlockData{{.*}} parent:0x[[#ROOT]]
block data
  integer, parameter :: n = 100
  integer, dimension(n) :: a, b, c
  common /arrays/ a, b, c
end

! CHECK: ModuleLike{{.*}} node:0x[[#%x, TEST_MOD:]] parent:0x[[#ROOT]]
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
  ! CHECK: Subroutine foo{{.*}} node:0x[[#%x, M_FOO:]] parent:0x[[#TEST_MOD]]
  subroutine foo()
    contains
    ! CHECK: Subroutine subfoo{{.*}} node:0x[[#%x, SUBFOO:]] parent:0x[[#M_FOO]]
    subroutine subfoo()
    end subroutine
    ! CHECK: Function subfoo2{{.*}} node:0x[[#%x, SUBFOO2:]] parent:0x[[#M_FOO]]
    function subfoo2()
    end function
  end subroutine

  ! CHECK: Function foo2{{.*}} node:0x[[#%x, M_FOO2:]] parent:0x[[#TEST_MOD]]
  function foo2(i, j)
    integer i, j, foo2
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#M_FOO2]]
    foo2 = i + j
    contains
    ! CHECK: Subroutine subfoo{{.*}} node:0x[[#%x, SUBFOO:]] parent:0x[[#M_FOO2]]
    subroutine subfoo()
    end subroutine
  end function
end module

! CHECK: ModuleLike{{.*}} node:0x[[#%x, SUB_MOD:]] parent:0x[[#ROOT]]
submodule (test_mod) test_mod_impl
contains
  ! CHECK: Subroutine foo{{.*}} node:0x[[#%x, SUBM_FOO:]] parent:0x[[#SUB_MOD]]
  subroutine foo()
    contains
    ! CHECK: Subroutine subfoo{{.*}} node:0x[[#%x, SUBFOO:]] parent:0x[[#SUBM_FOO]]
    subroutine subfoo()
    end subroutine
    ! CHECK: Function subfoo2{{.*}} node:0x[[#%x, SUBFOO2:]] parent:0x[[#SUBM_FOO]]
    function subfoo2()
    end function
  end subroutine
  ! CHECK: MpSubprogram dump{{.*}} node:0x[[#%x, MP_DUMP:]] parent:0x[[#SUB_MOD]]
  module procedure dump
    ! CHECK: FormatStmt{{.*}} parent:0x[[#MP_DUMP]]
11  format (2E16.4, I6)
    ! CHECK: <<IfConstruct>>{{.*}} node:0x[[#%x, IF1:]] parent:0x[[#MP_DUMP]]
    ! CHECK: IfThenStmt{{.*}} parent:0x[[#IF1]]
    if (xdim > 100) then
      ! CHECK: PrintStmt{{.*}} parent:0x[[#IF1]]
      print *, "test: ", xdim
    ! CHECK: ElseStmt{{.*}} parent:0x[[#IF1]]
    else
      ! CHECK: WriteStmt{{.*}} parent:0x[[#IF1]]
      write (*, 11) "test: ", xdim, pressure
    ! CHECK: EndIfStmt{{.*}} parent:0x[[#IF1]]
    end if
  end procedure
end submodule

! CHECK: BlockData{{.*}} parent:0x[[#ROOT]]
block data
 integer i, j, k
 common /indexes/ i, j, k
end

! CHECK: Function bar{{.*}} node:0x[[#%x, BAR:]] parent:0x[[#ROOT]]
function bar()
end function

! CHECK: Program <anonymous>{{.*}} node:0x[[#%x, PROG:]] parent:0x[[#ROOT]]
  ! check specification parts are not part of the PFT.
  ! CHECK-NOT: node
  use test_mod
  real, allocatable :: x(:)
  ! CHECK: AllocateStmt{{.*}} parent:0x[[#PROG]]
  allocate(x(foo2(10, 30)))
end
