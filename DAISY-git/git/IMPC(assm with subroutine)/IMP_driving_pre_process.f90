module IMP_driving_pre_process
    use IMP_assm_global
    implicit none
    private
    public::Sys_pre_process
contains
    subroutine Sys_pre_process()
     implicit none
     write(*,*)'start the sys pre process:'
     !读取参数
     call reInputdata%set()
     !call reInputdata%publish()
     !参数赋值
     call assm1%set(reInputdata)
     call timer1%set(150.0,150)!(ttotal,Nt)
     !分配空间
     call assm1%alloc()
     call timer1%alloc()
     !初始化
     call assm1%init()
     !    timer1%init(ttotal,Nt,ctime,ltime)
     call timer1%init(0.0,0.0)
    end subroutine Sys_pre_process
end module IMP_driving_pre_process