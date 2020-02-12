! RUN: %f18 -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test Pre-FIR Tree captures all the intended nodes from the parse-tree
! Coarray and OpenMP related nodes are tested in other files.

! CHECK: PFT root node:[[#%u, ROOT:]]
! CHECK: [[#%u, PROG:]]{{.*}}Program test_prog{{.*}}parent:[[#ROOT]]
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
  ! CHECK: [[#%u, DO1:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#PROG]]
  ! CHECK: NonLabelDoStmt{{.*}}parent:[[#DO1]]
  do i=1,5
    ! CHECK: PrintStmt{{.*}}parent:[[#DO1]]
    print *, "hey"
    ! CHECK: [[#%u, DO2:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#DO1]]
    ! CHECK: NonLabelDoStmt{{.*}}parent:[[#DO2]]
    do j=1,5
      ! CHECK: PrintStmt{{.*}}parent:[[#DO2]]
      print *, "hello", i, j
    ! CHECK: EndDoStmt{{.*}}parent:[[#DO2]]
    end do
  ! CHECK: EndDoStmt{{.*}}parent:[[#DO1]]
  end do

  ! CHECK: [[#%u, ASSOC:]]{{.*}}<<AssociateConstruct>>{{.*}}parent:[[#PROG]]
  ! CHECK: AssociateStmt{{.*}}parent:[[#ASSOC]]
  associate (k => i + j)
    ! CHECK: AllocateStmt{{.*}}parent:[[#ASSOC]]
    allocate(x(k))
  ! CHECK: EndAssociateStmt{{.*}}parent:[[#ASSOC]]
  end associate

  ! CHECK: [[#%u, BLOCK:]]{{.*}}<<BlockConstruct>>{{.*}}parent:[[#PROG]]
  ! CHECK: BlockStmt{{.*}}parent:[[#BLOCK]]
  block
    integer :: k, l
    real, pointer :: p(:)
    ! CHECK: PointerAssignmentStmt{{.*}}parent:[[#BLOCK]]
    p => x
    ! CHECK: AssignmentStmt{{.*}}parent:[[#BLOCK]]
    k = size(p)
    ! CHECK: AssignmentStmt{{.*}}parent:[[#BLOCK]]
    l = 1
    ! CHECK: [[#%u, SELECTCASE:]]{{.*}}<<CaseConstruct>>{{.*}}parent:[[#BLOCK]]
    ! CHECK: SelectCaseStmt{{.*}}parent:[[#SELECTCASE]]
    select case (k)
      ! CHECK: CaseStmt{{.*}}parent:[[#SELECTCASE]]
      case (:0)
        ! CHECK: NullifyStmt{{.*}}parent:[[#SELECTCASE]]
        nullify(p)
      ! CHECK: CaseStmt{{.*}}parent:[[#SELECTCASE]]
      case (1)
        ! CHECK: [[#%u, IFTHEN:]]{{.*}}<<IfConstruct>>{{.*}}parent:[[#SELECTCASE]]
        ! CHECK: IfThenStmt{{.*}}parent:[[#IFTHEN]]
        if (p(1)>0.) then
          ! CHECK: PrintStmt{{.*}}parent:[[#IFTHEN]]
          print *, "+"
        ! CHECK: ElseIfStmt{{.*}}parent:[[#IFTHEN]]
        else if (p(1)==0.) then
          ! CHECK: PrintStmt{{.*}}parent:[[#IFTHEN]]
          print *, "0."
        ! CHECK: ElseStmt{{.*}}parent:[[#IFTHEN]]
        else
          ! CHECK: PrintStmt{{.*}}parent:[[#IFTHEN]]
          print *, "-"
        ! CHECK: EndIfStmt{{.*}}parent:[[#IFTHEN]]
        end if
        ! CHECK: CaseStmt{{.*}}parent:[[#SELECTCASE]]
      case (2:10)
      ! CHECK: CaseStmt{{.*}}parent:[[#SELECTCASE]]
      case default
        ! Note: label-do-loop are canonicalized into do constructs
        ! CHECK: [[#%u, DO3:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#SELECTCASE]]
        ! CHECK: NonLabelDoStmt{{.*}}parent:[[#DO3]]
        do 22 while(l<=k)
          ! CHECK: IfStmt{{.*}}parent:[[#DO3]]
          if (p(l)<0.) p(l)=cos(p(l))
          ! CHECK: CallStmt{{.*}}parent:[[#DO3]]
22        call incr(l)
        ! CHECK: EndDoStmt{{.*}}parent:[[#DO3]]
      ! CHECK: CaseStmt{{.*}}parent:[[#SELECTCASE]]
      case (100:)
    ! CHECK: EndSelectStmt{{.*}}parent:[[#SELECTCASE]]
    end select
  ! CHECK: EndBlockStmt{{.*}}parent:[[#BLOCK]]
  end block

  ! CHECK-NOT: WhereConstruct
  ! CHECK: WhereStmt{{.*}}parent:[[#PROG]]
  where (x > 1.) x = x/2.

  ! CHECK: [[#%u, WHERE:]]{{.*}}<<WhereConstruct>>{{.*}}parent:[[#PROG]]
  ! CHECK: WhereConstructStmt{{.*}}parent:[[#WHERE]]
  where (x == 0.)
    ! CHECK: AssignmentStmt{{.*}}parent:[[#WHERE]]
    x = 0.01
  ! CHECK: MaskedElsewhereStmt{{.*}}parent:[[#WHERE]]
  elsewhere (x < 0.5)
    ! CHECK: AssignmentStmt{{.*}}parent:[[#WHERE]]
    x = x*2.
    ! CHECK: [[#%u, WHERE2:]]{{.*}}<<WhereConstruct>>{{.*}}parent:[[#WHERE]]
    where (y > 0.4)
      ! CHECK: AssignmentStmt{{.*}}parent:[[#WHERE2]]
      y = y/2.
    end where
  ! CHECK: ElsewhereStmt{{.*}}parent:[[#WHERE]]
  elsewhere
    ! CHECK: AssignmentStmt{{.*}}parent:[[#WHERE]]
    x = x + 1.
  ! CHECK: EndWhereStmt{{.*}}parent:[[#WHERE]]
  end where

  ! CHECK-NOT: ForAllConstruct
  ! CHECK: ForallStmt{{.*}}parent:[[#PROG]]
  forall (i = 1:5) x = y(i)

  ! CHECK: [[#%u, FORALL:]]{{.*}}<<ForallConstruct>>{{.*}}parent:[[#PROG]]
  ! CHECK: ForallConstructStmt{{.*}}parent:[[#FORALL]]
  forall (i = 1:5)
    ! CHECK: AssignmentStmt{{.*}}parent:[[#FORALL]]
    x(i) = x(i) + y(10*i)
  ! CHECK: EndForallStmt{{.*}}parent:[[#FORALL]]
  end forall

  ! CHECK: DeallocateStmt{{.*}}parent:[[#PROG]]
  deallocate(x)
end

! CHECK: [[#%u, MOD:]]{{.*}}ModuleLike{{.*}}parent:[[#ROOT]]
module test
  type :: a_type
    integer :: x
  end type
  type, extends(a_type) :: b_type
    integer :: y
  end type
contains
  ! CHECK: [[#%u, FOO:]]{{.*}}Function foo{{.*}}parent:[[#MOD]]
  function foo(x)
    real x(..)
    integer :: foo
    ! CHECK: [[#%u, SELECTRANK:]]{{.*}}<<SelectRankConstruct>>{{.*}}parent:[[#FOO]]
    ! CHECK: SelectRankStmt{{.*}}parent:[[#SELECTRANK]]
    select rank(x)
      ! CHECK: SelectRankCaseStmt{{.*}}parent:[[#SELECTRANK]]
      rank (0)
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTRANK]]
        foo = 0
      ! CHECK: SelectRankCaseStmt{{.*}}parent:[[#SELECTRANK]]
      rank (*)
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTRANK]]
        foo = -1
      ! CHECK: SelectRankCaseStmt{{.*}}parent:[[#SELECTRANK]]
      rank (1)
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTRANK]]
        foo = 1
      ! CHECK: SelectRankCaseStmt{{.*}}parent:[[#SELECTRANK]]
      rank default
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTRANK]]
        foo = 2
    ! CHECK: EndSelectStmt{{.*}}parent:[[#SELECTRANK]]
    end select
  end function

  ! CHECK: [[#%u, BAR:]]{{.*}}Function bar{{.*}}parent:[[#MOD]]
  function bar(x)
    class(*) :: x
    ! CHECK: [[#%u, SELECTTYPE:]]{{.*}}<<SelectTypeConstruct>>{{.*}}parent:[[#BAR]]
    ! CHECK: SelectTypeStmt{{.*}}parent:[[#SELECTTYPE]]
    select type(x)
      ! CHECK: TypeGuardStmt{{.*}}parent:[[#SELECTTYPE]]
      type is (integer)
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTTYPE]]
        bar = 0
      ! CHECK: TypeGuardStmt{{.*}}parent:[[#SELECTTYPE]]
      class is (a_type)
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTTYPE]]
        bar = 1
        ! CHECK: ReturnStmt{{.*}}parent:[[#SELECTTYPE]]
        return
      ! CHECK: TypeGuardStmt{{.*}}parent:[[#SELECTTYPE]]
      class default
        ! CHECK: AssignmentStmt{{.*}}parent:[[#SELECTTYPE]]
        bar = -1
    ! CHECK: EndSelectStmt{{.*}}parent:[[#SELECTTYPE]]
    end select
  end function

  ! CHECK: [[#%u, SUB:]]{{.*}}Subroutine sub{{.*}}parent:[[#MOD]]
  subroutine sub(a)
    real(4):: a
    ! CompilerDirective
    ! CHECK: <<CompilerDirective>>{{.*}}parent:[[#SUB]]
    !DIR$ IGNORE_TKR a
  end subroutine


end module

! CHECK: [[#%u, ALTSUB:]]{{.*}}Subroutine altreturn{{.*}}parent:[[#ROOT]]
subroutine altreturn(i, j, *, *)
  ! CHECK: [[#%u, IFTHEN:]]{{.*}}<<IfConstruct>>{{.*}}parent:[[#ALTSUB]]
  if (i>j) then
    ! CHECK: ReturnStmt{{.*}}parent:[[#IFTHEN]]
    return 1
  else
    ! CHECK: ReturnStmt{{.*}}parent:[[#IFTHEN]]
    return 2
  end if
end subroutine


! Remaining TODO

! CHECK: [[#%u, IO:]]{{.*}}Subroutine iostmts{{.*}}parent:[[#ROOT]]
subroutine iostmts(filename, a, b, c)
  character(*) :: filename
  integer :: length
  logical :: file_is_opened
  real, a, b ,c
  ! CHECK: InquireStmt{{.*}}parent:[[#]]
  inquire(file=filename, opened=file_is_opened)
  ! CHECK: [[#%u, IFTHEN:]]{{.*}}<<IfConstruct>>{{.*}}parent:[[#IO]]
  if (file_is_opened) then
    ! CHECK: OpenStmt{{.*}}parent:[[#IFTHEN]]
    open(10, FILE=filename)
  end if
  ! CHECK: ReadStmt{{.*}}parent:[[#IO]]
  read(10, *) length
  ! CHECK: RewindStmt{{.*}}parent:[[#IO]]
  rewind 10
  ! CHECK: NamelistStmt{{.*}}parent:[[#IO]]
  namelist /nlist/ a, b, c
  ! CHECK: WriteStmt{{.*}}parent:[[#IO]]
  write(10, NML=nlist)
  ! CHECK: BackspaceStmt{{.*}}parent:[[#IO]]
  backspace(10)
  ! CHECK: FormatStmt{{.*}}parent:[[#IO]]
1 format (1PE12.4)
  ! CHECK: WriteStmt{{.*}}parent:[[#IO]]
  write (10, 1) a
  ! CHECK: EndfileStmt{{.*}}parent:[[#IO]]
  endfile 10
  ! CHECK: FlushStmt{{.*}}parent:[[#IO]]
  flush 10
  ! CHECK: WaitStmt{{.*}}parent:[[#IO]]
  wait(10)
  ! CHECK: CloseStmt{{.*}}parent:[[#IO]]
  close(10)
end subroutine


! CHECK: [[#%u, SUB2:]]{{.*}}Subroutine sub2{{.*}}parent:[[#ROOT]]
subroutine sub2()
  integer :: i, j, k, l
  i = 0
1 j = i
  ! CHECK: ContinueStmt{{.*}}parent:[[#SUB2]]
2 continue
  i = i+1
3 j = j+1
! CHECK: ArithmeticIfStmt{{.*}}parent:[[#SUB2]]
  if (j-i) 3, 4, 5
  ! CHECK: GotoStmt{{.*}}parent:[[#SUB2]]
4  goto 6

! FIXME: is name resolution on assigned goto broken/todo ?
! WILLCHECK: AssignStmt{{.*}}parent:[[#SUB2]]
!55 assign 6 to label
! WILLCHECK: AssignedGotoStmt{{.*}}parent:[[#SUB2]]
!66  go to label (5, 6)

! CHECK: ComputedGotoStmt{{.*}}parent:[[#SUB2]]
  go to (5, 6), 1 + mod(i, 2)
5 j = j + 1
6 i = i + j/2

  ! CHECK: [[#%u, DO1:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#SUB2]]
  do1: do k=1,10
    ! CHECK: [[#%u, DO2:]]{{.*}}<<DoConstruct>>{{.*}}parent:[[#DO1]]
    do2: do l=5,20
      ! CHECK: CycleStmt{{.*}}parent:[[#DO2]]
      cycle do1
      ! CHECK: ExitStmt{{.*}}parent:[[#DO2]]
      exit do2
    end do do2
  end do do1

  ! CHECK: PauseStmt{{.*}}parent:[[#SUB2]]
  pause 7
  ! CHECK: StopStmt{{.*}}parent:[[#SUB2]]
  stop
end subroutine


! CHECK: [[#%u, SUB3:]]{{.*}}Subroutine sub3{{.*}}parent:[[#ROOT]]
subroutine sub3()
 print *, "normal"
  ! CHECK: EntryStmt{{.*}}parent:[[#SUB3]]
 entry sub4entry()
 print *, "test"
end subroutine

! CHECK: [[#%u, SUB4:]]{{.*}}Subroutine sub4{{.*}}parent:[[#ROOT]]
subroutine sub4(i, j)
  integer :: i
  print*, "test"
  ! CHECK: DataStmt{{.*}}parent:[[#SUB4]]
  data i /1/
end subroutine
