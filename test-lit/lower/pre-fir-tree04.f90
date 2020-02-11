! RUN: %f18_with_includes -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test Pre-FIR Tree captures all the coarray related statements

! CHECK: PFT root node:0x[[#%x, ROOT:]]
! CHECK: Subroutine test_coarray{{.*}} node:0x[[#%x, PROG:]] parent:0x[[#ROOT]]
Subroutine test_coarray
  use iso_fortran_env, only: team_type, event_type, lock_type
  type(team_type) :: t
  type(event_type) :: done
  type(lock_type) :: alock
  real :: y[10,*]
  integer :: counter[*]
  logical :: is_master
  ! CHECK: ChangeTeamConstruct{{.*}} node:0x[[#%x, CHANGE_TEAM:]] parent:0x[[#PROG]]
  change team(t, x[5,*] => y)
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#CHANGE_TEAM]]
    x = x[4, 1]
  end team
  ! CHECK: FormTeamStmt{{.*}} parent:0x[[#PROG]]
  form team(1, t)

  ! CHECK: IfConstruct{{.*}} node:0x[[#%x, IF:]] parent:0x[[#PROG]]
  if (this_image() == 1) then
    ! CHECK: EventPostStmt{{.*}} parent:0x[[#IF]]
    event post (done)
  else
    ! CHECK: EventWaitStmt{{.*}} parent:0x[[#IF]]
    event wait (done)
  end if

  ! CHECK: CriticalConstruct{{.*}} node:0x[[#%x, CRITICAL:]] parent:0x[[#PROG]]
  critical
    ! CHECK: AssignmentStmt{{.*}} parent:0x[[#CRITICAL]]
    counter[1] = counter[1] + 1
  end critical

  ! CHECK: LockStmt{{.*}} parent:0x[[#PROG]]
  lock(alock)
  ! CHECK: PrintStmt{{.*}} parent:0x[[#PROG]]
  print *, "I have the lock"
  ! CHECK: UnlockStmt{{.*}} parent:0x[[#PROG]]
  unlock(alock)

  ! CHECK: SyncAllStmt{{.*}} parent:0x[[#PROG]]
  sync all
  ! CHECK: SyncMemoryStmt{{.*}} parent:0x[[#PROG]]
  sync memory
  ! CHECK: SyncTeamStmt{{.*}} parent:0x[[#PROG]]
  sync team(t)

  ! CHECK: IfConstruct{{.*}} node:0x[[#%x, IF2:]] parent:0x[[#PROG]]
  if (this_image() == 1) then
    ! CHECK: SyncImagesStmt{{.*}} parent:0x[[#IF2]]
    sync images(*)
  else
    ! CHECK: SyncImagesStmt{{.*}} parent:0x[[#IF2]]
    sync images(1)
  end if

  ! CHECK: IfConstruct{{.*}} node:0x[[#%x, IF3:]] parent:0x[[#PROG]]
  if (y<0.) then
    ! CHECK: FailImageStmt{{.*}} parent:0x[[#IF3]]
   fail image
  end if
end
