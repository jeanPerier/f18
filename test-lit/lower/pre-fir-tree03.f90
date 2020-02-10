! RUN: %f18 -fdebug-pre-fir-tree -fparse-only -fopenmp %s | FileCheck %s

! Test Pre-FIR Tree captures OpenMP related constructs

! CHECK: PFT root node:[[#%u, ROOT:]]
! CHECK: [[#%u, PROG:]]{{.*}}Program test_omp{{.*}}parent:[[#ROOT]]
program test_omp
  ! CHECK: PrintStmt{{.*}}parent:[[#PROG]]
  print *, "sequential"

  ! CHECK: [[#%u, OMP_PAR:]]{{.*}}OpenMPConstruct{{.*}}parent:[[#PROG]]
  !$omp parallel

  ! CHECK: PrintStmt{{.*}}parent:[[#OMP_PAR]]
  print *, "in omp //"
  ! CHECK: [[#%u, OMP_LOOP:]]{{.*}}OpenMPConstruct{{.*}}parent:[[#OMP_PAR]]
  !$omp do
    ! CHECK: [[#%u, DO1:]]{{.*}}DoConstruct{{.*}}parent:[[#OMP_LOOP]]
    ! CHECK: LabelDoStmt{{.*}}parent:[[#DO1]]
    do i=1,100
      ! CHECK: PrintStmt{{.*}}parent:[[#DO1]]
      print *, "in omp do"
    ! CHECK: EndDoStmt{{.*}}parent:[[#DO1]]
    end do
  ! CHECK: OmpEndLoopDirective{{.*}}parent:[[#OMP_LOOP]]
  !$omp end do

  ! CHECK: PrintStmt{{.*}}parent:[[#OMP_PAR]]
  print *, "not in omp do"

  ! CHECK: [[#%u, OMP_LOOP2:]]{{.*}}OpenMPConstruct{{.*}}parent:[[#OMP_PAR]]
  !$omp do
    ! CHECK: [[#%u, DO2:]]{{.*}}DoConstruct{{.*}}parent:[[#OMP_LOOP2]]
    ! CHECK: LabelDoStmt{{.*}}parent:[[#DO2]]
    do i=1,100
      ! CHECK: PrintStmt{{.*}}parent:[[#DO2]]
      print *, "in omp do"
    ! CHECK: EndDoStmt{{.*}}parent:[[#DO2]]
    end do
    ! CHECK-NOT: OmpEndLoopDirective
    ! CHECK: PrintStmt{{.*}}parent:[[#OMP_PAR]]
    print *, "no in omp do"
  !$omp end parallel

  ! CHECK: PrintStmt{{.*}}parent:[[#PROG]]
  print *, "sequential again"

  ! CHECK: [[#%u, OMP_TASK:]]{{.*}}OpenMPConstruct{{.*}}parent:[[#PROG]]
  !$omp task
    ! CHECK: PrintStmt{{.*}}parent:[[#OMP_TASK]]
    print *, "in task"
  !$omp end task

  ! CHECK: PrintStmt{{.*}}parent:[[#PROG]]
  print *, "sequential again"
end program
