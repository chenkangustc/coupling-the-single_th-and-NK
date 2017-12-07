!***************************************************************************************
! test if
!  module: debugassembly
!
!  PURPOSE:  Entry point for the console application.
!
!  pow(na,nr),fq_core(na,nr)     ƽ�������ܶȣ����ʷ�����
!  nr = SIZE(assembly, dim=1)    ����������Ŀ
!  na = SIZE(assembly, dim=2)    ����Ľڿ���Ŀ��ԭ�����������Ҫ������ֵ�������þֲ��������
! 
!***************************************************************************************
    module IMP_th_interface
     !����ʵ����reInputdata/assm1
     use IMP_re_input_global
     use IMP_assm_global
     use IMP_driving_pre_process
     use IMP_driving_output
     use IMP_power_header
    implicit none

     real,allocatable::power(:,:),fq_core(:,:)
     integer M,N,i,j
    contains
    subroutine sys_thermal()
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
     allocate(temperature(M,N),pressure(M),velocity(M-1))
     !*********************************************
     call assm1%steady(power,fq_core)!power come from other data,so it should be an interface in place with the data
     temperature=assm1%thermal%temperature
     pressure=assm1%thermal%pressure
     velocity=assm1%thermal%velocity
     do while(timer1%ctime<timer1%ttotal) 
      timer1%ctime=timer1%ctime+timer1%dt
      call update_power(power,fq_core,timer1%ltime,timer1%ctime)
      call assm1%transient(power, fq_core,timer1%ltime,timer1%ctime)
      call timer1%record(assm1%th_boundary%T%outlet,assm1%th_boundary%u%inlet,power(1,1))
      print*,'ctime=',timer1%ctime
      timer1%ltime=timer1%ctime
     enddo
     call Run_output() 
     end subroutine sys_thermal
    end module IMP_th_interface

