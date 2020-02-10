! RUN: %f18_with_includes -fdebug-pre-fir-tree -fparse-only %s | FileCheck %s

! Test Pre-FIR Tree captures all the coarray related statements

! CHECK: PFT root node:[[#%u, ROOT:]]
! CHECK: [[#%u, PROG:]]{{.*}}Subroutine test_coarray{{.*}}parent:[[#ROOT]]
Subroutine test_coarray
  use iso_fortran_env, only: team_type, event_type, lock_type
  type(team_type) :: t
  type(event_type) :: done
  type(lock_type) :: alock
  real :: y[10,*]
  integer :: counter[*]
  logical :: is_master
  ! CHECK: [[#%u, CHANGE_TEAM:]]{{.*}}ChangeTeamConstruct{{.*}}parent:[[#PROG]]
  change team(t, x[5,*] => y)
    ! CHECK: AssignmentStmt{{.*}}parent:[[#CHANGE_TEAM]]
    x = x[4, 1]
  end team
  ! CHECK: FormTeamStmt{{.*}}parent:[[#PROG]]
  form team(1, t)

  ! CHECK: [[#%u, IF:]]{{.*}}IfConstruct{{.*}}parent:[[#PROG]]
  if (this_image() == 1) then
    ! CHECK: EventPostStmt{{.*}}parent:[[#IF]]
    event post (done)
  else
    ! CHECK: EventWaitStmt{{.*}}parent:[[#IF]]
    event wait (done)
  end if

  ! CHECK: [[#%u, CRITICAL:]]{{.*}}CriticalConstruct{{.*}}parent:[[#PROG]]
  critical
    ! CHECK: AssignmentStmt{{.*}}parent:[[#CRITICAL]]
    counter[1] = counter[1] + 1
  end critical

  ! CHECK: LockStmt{{.*}}parent:[[#PROG]]
  lock(alock)
  ! CHECK: PrintStmt{{.*}}parent:[[#PROG]]
  print *, "I have the lock"
  ! CHECK: UnlockStmt{{.*}}parent:[[#PROG]]
  unlock(alock)

  ! CHECK: SyncAllStmt{{.*}}parent:[[#PROG]]
  sync all
  ! CHECK: SyncMemoryStmt{{.*}}parent:[[#PROG]]
  sync memory
  ! CHECK: SyncTeamStmt{{.*}}parent:[[#PROG]]
  sync team(t)

  ! CHECK: [[#%u, IF2:]]{{.*}}IfConstruct{{.*}}parent:[[#PROG]]
  if (this_image() == 1) then
    ! CHECK: SyncImagesStmt{{.*}}parent:[[#IF2]]
    sync images(*)
  else
    ! CHECK: SyncImagesStmt{{.*}}parent:[[#IF2]]
    sync images(1)
  end if

  ! CHECK: [[#%u, IF3:]]{{.*}}IfConstruct{{.*}}parent:[[#PROG]]
  if (y<0.) then
    ! CHECK: FailImageStmt{{.*}}parent:[[#IF3]]
   fail image
  end if
end
