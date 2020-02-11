! RUN: %f18 -fdebug-pre-fir-tree -fparse-only -fopenmp %s | FileCheck %s

! Test Pre-FIR Tree captures OpenMP related constructs

! CHECK: PFT root node:0x[[#%x, ROOT:]]
! CHECK: Program test_omp{{.*}} node:0x[[#%x, PROG:]] parent:0x[[#ROOT]]
program test_omp
  ! CHECK: PrintStmt{{.*}} parent:0x[[#PROG]]
  print *, "sequential"

  ! CHECK: OpenMPConstruct{{.*}} node:0x[[#%x, OMP_PAR:]] parent:0x[[#PROG]]
  !$omp parallel

  ! CHECK: PrintStmt{{.*}} parent:0x[[#OMP_PAR]]
  print *, "in omp //"
  ! CHECK: OpenMPConstruct{{.*}} node:0x[[#%x, OMP_LOOP:]] parent:0x[[#OMP_PAR]]
  !$omp do
    ! CHECK: DoConstruct{{.*}} node:0x[[#%x, DO1:]] parent:0x[[#OMP_LOOP]]
    ! CHECK: LabelDoStmt{{.*}} parent:0x[[#DO1]]
    do i=1,100
      ! CHECK: PrintStmt{{.*}} parent:0x[[#DO1]]
      print *, "in omp do"
    ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO1]]
    end do
  ! CHECK: OmpEndLoopDirective{{.*}} parent:0x[[#OMP_LOOP]]
  !$omp end do

  ! CHECK: PrintStmt{{.*}} parent:0x[[#OMP_PAR]]
  print *, "not in omp do"

  ! CHECK: OpenMPConstruct{{.*}} node:0x[[#%x, OMP_LOOP2:]] parent:0x[[#OMP_PAR]]
  !$omp do
    ! CHECK: DoConstruct{{.*}} node:0x[[#%x, DO2:]] parent:0x[[#OMP_LOOP2]]
    ! CHECK: LabelDoStmt{{.*}} parent:0x[[#DO2]]
    do i=1,100
      ! CHECK: PrintStmt{{.*}} parent:0x[[#DO2]]
      print *, "in omp do"
    ! CHECK: EndDoStmt{{.*}} parent:0x[[#DO2]]
    end do
    ! CHECK-NOT: OmpEndLoopDirective
    ! CHECK: PrintStmt{{.*}} parent:0x[[#OMP_PAR]]
    print *, "no in omp do"
  !$omp end parallel

  ! CHECK: PrintStmt{{.*}} parent:0x[[#PROG]]
  print *, "sequential again"

  ! CHECK: OpenMPConstruct{{.*}} node:0x[[#%x, OMP_TASK:]] parent:0x[[#PROG]]
  !$omp task
    ! CHECK: PrintStmt{{.*}} parent:0x[[#OMP_TASK]]
    print *, "in task"
  !$omp end task

  ! CHECK: PrintStmt{{.*}} parent:0x[[#PROG]]
  print *, "sequential again"
end program
