module IMP_assm_global
    use IMP_assembly_header
    use IMP_timer_header
    implicit none
    type(sys_assembly)::assm1
    type(sys_timer)::timer1
    real,allocatable::temperature(:,:)
    real,allocatable::pressure(:)
    real,allocatable::velocity(:)
end module IMP_assm_global