! RUN: %f18 -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test Pre-FIR Tree captures all the intended nodes from the parse-tree
! Coarray and OpenMP related nodes are tested in other files.

! CHECK: PFT root node:0x[[#%x, ROOT:]]
! CHECK: Program test_prog{{.*}} node:0x[[#%x, PROG:]] parent:0x[[#ROOT]]
program test_prog
  ! Check specification part is not part of the tree.
  interface
    subroutine incr(i)
      integer, intent(inout) :: i
    end subroutine
  end interface
  integer :: i, j, k
  real, allocatable, target :: x(:)
  real :: y(100)
  ! CHECK-NOT: node
  ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO1:]] parent:0x[[#PROG]]
  ! CHECK: NonLabelDoStmt{{.*}} parent:0x[[#DO1]]
  do i=1,5
    ! CHECK: PrintStmt{{.*}} parent:0x[[#DO1]]
    print *, "hey"
    ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO2:]] parent:0x[[#DO1]]
    ! CHECK: NonLabelDoStmt{{.*}} parent:0x[[#DO2]]
    do j=1,5
      ! CHECK: PrintStmt{{.*}} parent:0x[[#DO2]]
      print *, "hello", i, j
    ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO2]]
    end do
  ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO1]]
  end do

  ! CHECK: <<AssociateConstruct>>{{.*}} node:0x[[#%x, ASSOC:]] parent:0x[[#PROG]]
  ! CHECK: AssociateStmt{{.*}} parent:0x[[#ASSOC]]
  associate (k => i + j)
    ! CHECK: AllocateStmt{{.*}} parent:0x[[#ASSOC]]
    allocate(x(k))
  ! CHECK: EndAssociateStmt{{.*}} parent:0x[[#ASSOC]]
  end associate

  ! CHECK: <<BlockConstruct>>{{.*}} node:0x[[#%x, BLOCK:]] parent:0x[[#PROG]]
  ! CHECK: BlockStmt{{.*}} parent:0x[[#BLOCK]]
  block
    integer :: k, l
    real, pointer :: p(:)
    ! CHECK: PointerAssignmentStmt{{.*}} parent:0x[[#BLOCK]]
    p => x
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#BLOCK]]
    k = size(p)
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#BLOCK]]
    l = 1
    ! CHECK: <<CaseConstruct>>{{.*}} node:0x[[#%x, SELECTCASE:]] parent:0x[[#BLOCK]]
    ! CHECK: SelectCaseStmt{{.*}} parent:0x[[#SELECTCASE]]
    select case (k)
      ! CHECK: CaseStmt{{.*}} parent:0x[[#SELECTCASE]]
      case (:0)
        ! CHECK: NullifyStmt{{.*}} parent:0x[[#SELECTCASE]]
        nullify(p)
      ! CHECK: CaseStmt{{.*}} parent:0x[[#SELECTCASE]]
      case (1)
        ! CHECK: <<IfConstruct>>{{.*}} node:0x[[#%x, IFTHEN:]] parent:0x[[#SELECTCASE]]
        ! CHECK: IfThenStmt{{.*}} parent:0x[[#IFTHEN]]
        if (p(1)>0.) then
          ! CHECK: PrintStmt{{.*}} parent:0x[[#IFTHEN]]
          print *, "+"
        ! CHECK: ElseIfStmt{{.*}} parent:0x[[#IFTHEN]]
        else if (p(1)==0.) then
          ! CHECK: PrintStmt{{.*}} parent:0x[[#IFTHEN]]
          print *, "0."
        ! CHECK: ElseStmt{{.*}} parent:0x[[#IFTHEN]]
        else
          ! CHECK: PrintStmt{{.*}} parent:0x[[#IFTHEN]]
          print *, "-"
        ! CHECK: EndIfStmt{{.*}} parent:0x[[#IFTHEN]]
        end if
        ! CHECK: CaseStmt{{.*}} parent:0x[[#SELECTCASE]]
      case (2:10)
      ! CHECK: CaseStmt{{.*}} parent:0x[[#SELECTCASE]]
      case default
        ! Note: label-do-loop are canonicalized into do constructs
        ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO3:]] parent:0x[[#SELECTCASE]]
        ! CHECK: NonLabelDoStmt{{.*}} parent:0x[[#DO3]]
        do 22 while(l<=k)
          ! CHECK: IfStmt{{.*}} parent:0x[[#DO3]]
          if (p(l)<0.) p(l)=cos(p(l))
          ! CHECK: CallStmt{{.*}} parent:0x[[#DO3]]
22        call incr(l)
        ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO3]]
      ! CHECK: CaseStmt{{.*}} parent:0x[[#SELECTCASE]]
      case (100:)
    ! CHECK: EndSelectStmt{{.*}} parent:0x[[#SELECTCASE]]
    end select
  ! CHECK: EndBlockStmt{{.*}} parent:0x[[#BLOCK]]
  end block

  ! CHECK-NOT: WhereConstruct
  ! CHECK: WhereStmt{{.*}} parent:0x[[#PROG]]
  where (x > 1.) x = x/2.

  ! CHECK: <<WhereConstruct>>{{.*}} node:0x[[#%x, WHERE:]] parent:0x[[#PROG]]
  ! CHECK: WhereConstructStmt{{.*}} parent:0x[[#WHERE]]
  where (x == 0.)
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#WHERE]]
    x = 0.01
  ! CHECK: MaskedElsewhereStmt{{.*}} parent:0x[[#WHERE]]
  elsewhere (x < 0.5)
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#WHERE]]
    x = x*2.
    ! CHECK: <<WhereConstruct>>{{.*}} node:0x[[#%x, WHERE2:]] parent:0x[[#WHERE]]
    where (y > 0.4)
      ! CHECK: AssignmentStmt{{.*}} parent:0x[[#WHERE2]]
      y = y/2.
    end where
  ! CHECK: ElsewhereStmt{{.*}} parent:0x[[#WHERE]]
  elsewhere
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#WHERE]]
    x = x + 1.
  ! CHECK: EndWhereStmt{{.*}} parent:0x[[#WHERE]]
  end where

  ! CHECK-NOT: ForAllConstruct
  ! CHECK: ForallStmt{{.*}} parent:0x[[#PROG]]
  forall (i = 1:5) x = y(i)

  ! CHECK: <<ForallConstruct>>{{.*}} node:0x[[#%x, FORALL:]] parent:0x[[#PROG]]
  ! CHECK: ForallConstructStmt{{.*}} parent:0x[[#FORALL]]
  forall (i = 1:5)
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#FORALL]]
    x(i) = x(i) + y(10*i)
  ! CHECK: EndForallStmt{{.*}} parent:0x[[#FORALL]]
  end forall

  ! CHECK: DeallocateStmt{{.*}} parent:0x[[#PROG]]
  deallocate(x)
end

! CHECK: ModuleLike{{.*}} node:0x[[#%x, MOD:]] parent:0x[[#ROOT]]
module test
  type :: a_type
    integer :: x
  end type
  type, extends(a_type) :: b_type
    integer :: y
  end type
contains
  ! CHECK: Function foo{{.*}} node:0x[[#%x, FOO:]] parent:0x[[#MOD]]
  function foo(x)
    real x(..)
    integer :: foo
    ! CHECK: <<SelectRankConstruct>>{{.*}} node:0x[[#%x, SELECTRANK:]] parent:0x[[#FOO]]
    ! CHECK: SelectRankStmt{{.*}} parent:0x[[#SELECTRANK]]
    select rank(x)
      ! CHECK: SelectRankCaseStmt{{.*}} parent:0x[[#SELECTRANK]]
      rank (0)
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTRANK]]
        foo = 0
      ! CHECK: SelectRankCaseStmt{{.*}} parent:0x[[#SELECTRANK]]
      rank (*)
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTRANK]]
        foo = -1
      ! CHECK: SelectRankCaseStmt{{.*}} parent:0x[[#SELECTRANK]]
      rank (1)
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTRANK]]
        foo = 1
      ! CHECK: SelectRankCaseStmt{{.*}} parent:0x[[#SELECTRANK]]
      rank default
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTRANK]]
        foo = 2
    ! CHECK: EndSelectStmt{{.*}} parent:0x[[#SELECTRANK]]
    end select
  end function

  ! CHECK: Function bar{{.*}} node:0x[[#%x, BAR:]] parent:0x[[#MOD]]
  function bar(x)
    class(*) :: x
    ! CHECK: <<SelectTypeConstruct>>{{.*}} node:0x[[#%x, SELECTTYPE:]] parent:0x[[#BAR]]
    ! CHECK: SelectTypeStmt{{.*}} parent:0x[[#SELECTTYPE]]
    select type(x)
      ! CHECK: TypeGuardStmt{{.*}} parent:0x[[#SELECTTYPE]]
      type is (integer)
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTTYPE]]
        bar = 0
      ! CHECK: TypeGuardStmt{{.*}} parent:0x[[#SELECTTYPE]]
      class is (a_type)
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTTYPE]]
        bar = 1
        ! CHECK: ReturnStmt{{.*}} parent:0x[[#SELECTTYPE]]
        return
      ! CHECK: TypeGuardStmt{{.*}} parent:0x[[#SELECTTYPE]]
      class default
        ! CHECK: AssignmentStmt{{.*}} parent:0x[[#SELECTTYPE]]
        bar = -1
    ! CHECK: EndSelectStmt{{.*}} parent:0x[[#SELECTTYPE]]
    end select
  end function

  ! CHECK: Subroutine sub{{.*}} node:0x[[#%x, SUB:]] parent:0x[[#MOD]]
  subroutine sub(a)
    real(4):: a
    ! CompilerDirective
    ! CHECK: <<CompilerDirective>>{{.*}} parent:0x[[#SUB]]
    !DIR$ IGNORE_TKR a
  end subroutine


end module

! CHECK: Subroutine altreturn{{.*}} node:0x[[#%x, ALTSUB:]] parent:0x[[#ROOT]]
subroutine altreturn(i, j, *, *)
  ! CHECK: <<IfConstruct>>{{.*}} node:0x[[#%x, IFTHEN:]] parent:0x[[#ALTSUB]]
  if (i>j) then
    ! CHECK: ReturnStmt{{.*}} parent:0x[[#IFTHEN]]
    return 1
  else
    ! CHECK: ReturnStmt{{.*}} parent:0x[[#IFTHEN]]
    return 2
  end if
end subroutine


! Remaining TODO

! CHECK: Subroutine iostmts{{.*}} node:0x[[#%x, IO:]] parent:0x[[#ROOT]]
subroutine iostmts(filename, a, b, c)
  character(*) :: filename
  integer :: length
  logical :: file_is_opened
  real, a, b ,c
  ! CHECK: InquireStmt{{.*}} parent:0x[[#]]
  inquire(file=filename, opened=file_is_opened)
  ! CHECK: <<IfConstruct>>{{.*}} node:0x[[#%x, IFTHEN:]] parent:0x[[#IO]]
  if (file_is_opened) then
    ! CHECK: OpenStmt{{.*}} parent:0x[[#IFTHEN]]
    open(10, FILE=filename)
  end if
  ! CHECK: ReadStmt{{.*}} parent:0x[[#IO]]
  read(10, *) length
  ! CHECK: RewindStmt{{.*}} parent:0x[[#IO]]
  rewind 10
  ! CHECK: NamelistStmt{{.*}} parent:0x[[#IO]]
  namelist /nlist/ a, b, c
  ! CHECK: WriteStmt{{.*}} parent:0x[[#IO]]
  write(10, NML=nlist)
  ! CHECK: BackspaceStmt{{.*}} parent:0x[[#IO]]
  backspace(10)
  ! CHECK: FormatStmt{{.*}} parent:0x[[#IO]]
1 format (1PE12.4)
  ! CHECK: WriteStmt{{.*}} parent:0x[[#IO]]
  write (10, 1) a
  ! CHECK: EndfileStmt{{.*}} parent:0x[[#IO]]
  endfile 10
  ! CHECK: FlushStmt{{.*}} parent:0x[[#IO]]
  flush 10
  ! CHECK: WaitStmt{{.*}} parent:0x[[#IO]]
  wait(10)
  ! CHECK: CloseStmt{{.*}} parent:0x[[#IO]]
  close(10)
end subroutine


! CHECK: Subroutine sub2{{.*}} node:0x[[#%x, SUB2:]] parent:0x[[#ROOT]]
subroutine sub2()
  integer :: i, j, k, l
  i = 0
1 j = i
  ! CHECK: ContinueStmt{{.*}} parent:0x[[#SUB2]]
2 continue
  i = i+1
3 j = j+1
! CHECK: ArithmeticIfStmt{{.*}} parent:0x[[#SUB2]]
  if (j-i) 3, 4, 5
  ! CHECK: GotoStmt{{.*}} parent:0x[[#SUB2]]
4  goto 6

! FIXME: is name resolution on assigned goto broken/todo ?
! WILLCHECK: AssignStmt{{.*}} parent:0x[[#SUB2]]
!55 assign 6 to label
! WILLCHECK: AssignedGotoStmt{{.*}} parent:0x[[#SUB2]]
!66  go to label (5, 6)

! CHECK: ComputedGotoStmt{{.*}} parent:0x[[#SUB2]]
  go to (5, 6), 1 + mod(i, 2)
5 j = j + 1
6 i = i + j/2

  ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO1:]] parent:0x[[#SUB2]]
  do1: do k=1,10
    ! CHECK: <<DoConstruct>>{{.*}} node:0x[[#%x, DO2:]] parent:0x[[#DO1]]
    do2: do l=5,20
      ! CHECK: CycleStmt{{.*}} parent:0x[[#DO2]]
      cycle do1
      ! CHECK: ExitStmt{{.*}} parent:0x[[#DO2]]
      exit do2
    end do do2
  end do do1

  ! CHECK: PauseStmt{{.*}} parent:0x[[#SUB2]]
  pause 7
  ! CHECK: StopStmt{{.*}} parent:0x[[#SUB2]]
  stop
end subroutine


! CHECK: Subroutine sub3{{.*}} node:0x[[#%x, SUB3:]] parent:0x[[#ROOT]]
subroutine sub3()
 print *, "normal"
  ! CHECK: EntryStmt{{.*}} parent:0x[[#SUB3]]
 entry sub4entry()
 print *, "test"
end subroutine

! CHECK: Subroutine sub4{{.*}} node:0x[[#%x, SUB4:]] parent:0x[[#ROOT]]
subroutine sub4(i, j)
  integer :: i
  print*, "test"
  ! CHECK: DataStmt{{.*}} parent:0x[[#SUB4]]
  data i /1/
end subroutine
