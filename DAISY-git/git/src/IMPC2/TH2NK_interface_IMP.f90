!***************************************************************************************
! test if
!  module: debugassembly
!
!  PURPOSE:  Entry point for the console application.
!
!  pow(na,nr),fq_core(na,nr)     平均功率密度，功率峰因子
!  nr = SIZE(assembly, dim=1)    径向的组件数目
!  na = SIZE(assembly, dim=2)    轴向的节块数目，原输入变量不需要操作赋值的另外用局部变量表达
! 
!***************************************************************************************
    module TH2NK_interface_IMP
      use imp_re_input_global
     use imp_assm_global
     use imp_driving_pre_process
     use imp_driving_output
     use imp_power_header
     use imp_single_channel
    implicit none

     real,allocatable::power(:,:),fq_core(:,:)
     integer M,N,i,j
    contains
    subroutine Perform_TH_imp()
     call sys_pre_process()
     !*********************************************
     M=size(assm1%thermal%temperature,dim=1)
     N=size(assm1%thermal%temperature,dim=2)
     allocate(power(M,N),fq_core(M,N))
     fq_core=0.0
     power=0.0  
     do i=1,M,1
         do j=1,N,1
          if(j<=assm1%mesh%Nf) power(i,j)=2.827*1e7
         enddo
     enddo
     !*********************************************
     call driving_imp_steady(assm1,power,fq_core)!power come from other data,so it should be an interface in place with the data

     do while(timer1%ctime<timer1%ttotal) 
      timer1%ctime=timer1%ctime+timer1%dt
      call update_power(power,fq_core,timer1%ltime,timer1%ctime)
      call driving_imp_transient(assm1,power, fq_core,timer1%ltime,timer1%ctime)
      call timer1%record(assm1%th_boundary%T%outlet,assm1%th_boundary%u%inlet,power(1,1))
      print*,'ctime=',timer1%ctime
      timer1%ltime=timer1%ctime
     enddo
     call Run_output() 
     read(*,*)
     end subroutine Perform_TH_imp
    end module TH2NK_interface_IMP

