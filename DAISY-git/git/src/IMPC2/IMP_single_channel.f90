module imp_single_channel
    use imp_assm_global
    use imp_single_kernel
    use imp_assembly_header
    implicit none
     private
     public::driving_imp_steady
     public::driving_imp_Transient
    contains
    subroutine driving_imp_steady(assm,power,fq_core)
    type(sys_assembly),intent(in out)::assm
    real,intent(in)::power(:,:)
    real,intent(in)::fq_core(:,:)
    !local rhoi/ui/Ti/dt/ap/pmodify/
    integer M,N,Ny,i,j
    real flag,dt
    real btotal,drho!判断因子
    real,allocatable::pmodify(:)
    real,allocatable::ui(:),Ti(:,:)
    real,allocatable::rhoi(:,:),rhofi(:)
    real,allocatable::ap(:)
    write(*,*)'start steady calculation:'
    flag=0.0
    dt=1.0!为保证方程求解dt不为0，无具体意义+
    Ny=assm%mesh%Ny
    M=Ny+1
    N=assm%mesh%Nf+assm%mesh%Ng+assm%mesh%Ns+1
    allocate(pmodify(Ny),ui(Ny-1),Ti(M-1,N),rhoi(0:M,0:N),rhofi(0:M),ap(Ny-1))
 
    pmodify=0.0
    ui=assm%thermal%velocity
    Ti=assm%thermal%temperature
    rhoi=assm%property%rho
    rhofi=assm%property%rho(:,N)
    ap=0.0
    j=0
    drho=1.0
    call assm%pow%set(power,fq_core)
    do while(drho>assm%confactor_%alpha)  
       i=0
       btotal=1.0
      do while(btotal>assm%confactor_%sigma)
	   i=i+1
       call solve_momentum(assm,flag,rhofi,ui,dt,ap)!需要输入当前迭代步的rho,uz 
       call solve_pressureCorrection(assm,flag,ap,rhofi,dt,pmodify,btotal)
       call modify_PV(assm,ap,pmodify)
       print*,'pv step=',i,' btotal=',btotal
      end do
      j=j+1
      call solve_temperature(assm,flag,Ti,rhoi,dt)
      call update_property(assm,drho)!物性更新
      print*,'property step=',j,' drho=',drho
    end do
  end subroutine driving_imp_steady
     
subroutine driving_imp_Transient(assm,power,fq_core,ltime,ctime)
    type(sys_assembly),intent(in out)::assm
    real,intent(in)::power(:,:)
    real,intent(in)::fq_core(:,:)
    real,intent(in)::ltime
    real,intent(in)::ctime
    !local rhoi/ui/Ti/dt/ap/pmodify/
    integer M,N,Ny,i,j
    real dt
    real flag
    real btotal,drho!判断因子
    real,allocatable::pmodify(:)
    real,allocatable::ui(:),Ti(:,:)
    real,allocatable::rhoi(:,:),rhofi(:)
    real,allocatable::ap(:)
    write(*,*)'start transient calculation:'
    flag=1.0
    dt=ctime-ltime
    Ny=assm%mesh%Ny
    M=Ny+1
    N=assm%mesh%Nf+assm%mesh%Ng+assm%mesh%Ns+1
    allocate(pmodify(Ny),ui(Ny-1),Ti(M-1,N),rhoi(0:M,0:N),rhofi(0:M),ap(Ny-1))
    
    pmodify=0.0
    Ti=assm%thermal%temperature
    ui=assm%thermal%velocity
    rhoi=assm%property%rho
    rhofi=assm%property%rho(:,N)
    ap=0.0
    call assm%th_boundary%update(ctime)
    call assm%pow%set(power,fq_core)
    j=0
    drho=1.0
    do while(drho>assm%confactor_%alpha)
       i=0
       btotal=1.0
      do while(btotal>assm%confactor_%sigma)
       i=i+1
       call solve_momentum(assm,flag,rhofi,ui,dt,ap)!需要输入当前迭代步的rho,uz 
       call solve_pressureCorrection(assm,flag,ap,rhofi,dt,pmodify,btotal)
       call modify_PV(assm,ap,pmodify)
       print*,'pv step=',i,' btotal=',btotal
      end do
      j=j+1     
      call solve_temperature(assm,flag,Ti,rhoi,dt)
      call update_property(assm,drho)!物性更新
      print*,'density step=',j,' drho=',drho
    end do
  end subroutine driving_imp_Transient
end module imp_single_channel