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
    module TH2NK_interface_IMP
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
    subroutine Perform_TH_imp(transient_flag, assembly, Tfuel, Tcoolant, Rhocoolant, max_Tfuel, max_Tcoolant, min_Rhocoolant, last, current, toutlet)
        logical, intent(in)      :: transient_flag                              ! .TRUE. --transient
        real(KREAL), intent(in)  :: assembly(:, :)                              ! (nr, na), in W, ���������;
        real(KREAL), intent(in out)  :: Tfuel(:, :)                             ! (nr, na), in K, �����ƽ��ȼ���¶�;
        real(KREAL), intent(in out)  :: Tcoolant(:, :)                          ! (nr, na), in K, �����ƽ����ȴ���¶�;
        real(KREAL), intent(in out)  :: Rhocoolant(:, :)                        ! (nr, na), in Kg/m^3, �����ƽ����ȴ���ܶ�;
        real(KREAL), intent(in out)  :: max_Tfuel                               ! in K, ����������ȼ���¶�;
        real(KREAL), intent(in out)  :: max_Tcoolant                            ! in K, ������������ȴ���¶�;
        real(KREAL), intent(in out)  :: min_Rhocoolant                          ! in Kg/m^3, ������������ȴ���ܶ�;
        real(KREAL), intent(in)  :: last                                        ! in s, ��һʱ���
        real(KREAL), intent(in)  :: current                                     ! in s, ��ǰʱ���
        real(KREAL), intent(in out)  :: toutlet                                 ! in K, ��ȴ������ƽ���¶�
    
        !
        REAL(KREAL)  :: last_, current_
        real(KREAL), allocatable  :: power(:, :)
        real(KREAL), allocatable  :: fq_core(:, :)
        integer  :: nr, na, npin
        integer  :: ir, ia, ipin, itype
        integer  :: i_allocate
        
        last_ = last
        current_ = current
        nr = SIZE(assembly, dim=1)                                              ! ����������Ŀ
        na = SIZE(assembly, dim=2)                                              ! ����Ľڿ���Ŀ
        
        allocate(power(na, nr), stat=i_allocate)
        allocate(fq_core(na, nr), stat=i_allocate)
        power = 0.0
        
        do ir = 1, nr
            itype = geom_th%geom_type(ir)
            if (itype > 0)  then
                do ia = 1, na
                    power(ia, ir) = assembly(ir, ia)
                end do
            end if
        end do
        
        fq_core = 1.0D0     
        if (transient_flag)  then
            !call Driving_ParallelChannel_transient (power, fq_core, 1, last_, current_)
        else
            call Driving_ParallelChannel_steady (power, fq_core)
        end if    
    
    
    
    !*********************************************
    !*********************************************
    !********************************************* 
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
     end subroutine Perform_TH_imp
    end module TH2NK_interface_IMP

