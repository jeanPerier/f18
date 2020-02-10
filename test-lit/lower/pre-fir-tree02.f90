! RUN: %f18 -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test Pre-FIR Rree captures all the intended nodes from the parse-tree

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
  ! FIXME: assignment not captured here ?
    x = 0.01
  ! CHECK: MaskedElsewhereStmt{{.*}} parent:0x[[#WHERE]]
  elsewhere (x < 0.5)
    x = x*2.
  ! CHECK: ElsewhereStmt{{.*}} parent:0x[[#WHERE]]
  elsewhere
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

! CHECK: Subroutine iocheck{{.*}} node:0x[[#%x, IO:]] parent:0x[[#ROOT]]
subroutine iocheck()
! WILLCHECK: BackspaceStmt{{.*}} parent:0x[[#]]
! WILLCHECK: CloseStmt{{.*}} parent:0x[[#]]
! WILLCHECK: OpenStmt{{.*}} parent:0x[[#]]
! WILLCHECK: PauseStmt{{.*}} parent:0x[[#]]
! WILLCHECK: ReadStmt{{.*}} parent:0x[[#]]
! WILLCHECK: RewindStmt{{.*}} parent:0x[[#]]
! WILLCHECK: WriteStmt{{.*}} parent:0x[[#]]
! WILLCHECK: InquireStmt{{.*}} parent:0x[[#]]
! WILLCHECK: WaitStmt{{.*}} parent:0x[[#]]
! WILLCHECK: EndfileStmt{{.*}} parent:0x[[#]]
! WILLCHECK: FlushStmt{{.*}} parent:0x[[#]]
end subroutine

! CHECK: Subroutine sub2{{.*}} node:0x[[#%x, SUB2:]] parent:0x[[#ROOT]]
subroutine sub2()
! WILLCHECK: ArithmeticIfStmt{{.*}} parent:0x[[#]]
! WILLCHECK: AssignedGotoStmt{{.*}} parent:0x[[#]]
! WILLCHECK: AssignStmt{{.*}} parent:0x[[#]]
! WILLCHECK: ComputedGotoStmt{{.*}} parent:0x[[#]]
! WILLCHECK: ContinueStmt{{.*}} parent:0x[[#]]
! WILLCHECK: CycleStmt{{.*}} parent:0x[[#]]
! WILLCHECK: ExitStmt{{.*}} parent:0x[[#]]
! WILLCHECK: GotoStmt{{.*}} parent:0x[[#]]
! WILLCHECK: StopStmt{{.*}} parent:0x[[#]]
end subroutine

! TODO: check others

! TODO: openmp related tests
!      common::Indirection<OpenMPConstruct>,
!      common::Indirection<OmpEndLoopDirective>>

! TODO: coarray related test
!      common::Indirection<ChangeTeamConstruct>,
!      common::Indirection<CriticalConstruct>,
! WILLCHECK: EventPostStmt{{.*}} parent:0x[[#]]
! WILLCHECK: EventWaitStmt{{.*}} parent:0x[[#]]
! WILLCHECK: FormTeamStmt{{.*}} parent:0x[[#]]
! WILLCHECK: FailImageStmt{{.*}} parent:0x[[#]]
! WILLCHECK: LockStmt{{.*}} parent:0x[[#]]
! WILLCHECK: SyncAllStmt{{.*}} parent:0x[[#]]
! WILLCHECK: SyncImagesStmt{{.*}} parent:0x[[#]]
! WILLCHECK: SyncMemoryStmt{{.*}} parent:0x[[#]]
! WILLCHECK: SyncTeamStmt{{.*}} parent:0x[[#]]
! WILLCHECK: UnlockStmt{{.*}} parent:0x[[#]]
