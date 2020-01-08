! Simple character assignment tests

subroutine assign1(s1, s2)
 character(*, 1) :: s1, s2
 s1 = s2
end subroutine

subroutine assign2(s1, s2)
 character(*, 2) :: s1, s2
 s1 = s2
end subroutine

subroutine assign4(s1, s2)
 character(*, 4) :: s1, s2
 s1 = s2
end subroutine
