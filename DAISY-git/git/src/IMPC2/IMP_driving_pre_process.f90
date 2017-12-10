module imp_driving_pre_process
    use imp_assm_global
    use imp_re_input_global
    implicit none
    private
    public::Sys_pre_process
    public::alloc_assembly
    public::free_assembly
    public::set_assembly
    public::init_assembly
    public::cal_grid
contains
    subroutine Sys_pre_process()
     implicit none
     write(*,*)'start the sys pre process:'
     !读取参数
     call reInputdata%set()
     !call reInputdata%publish()
     !参数赋值
     call set_assembly(assm1,reInputdata)
	 print*,'set nf ng ns...'
	 assm1%mesh%Nf=reInputdata%Nf
	 assm1%mesh%Ng=reInputdata%Ng
	 assm1%mesh%Ns=reInputdata%Ns
	 print*,'nf=',assm1%mesh%nf,'ng=',assm1%mesh%ng,'ns=',assm1%mesh%ns
     call timer1%set(150.0,150)!(ttotal,Nt)
     !分配空间
     call alloc_assembly(assm1)
     call timer1%alloc()
     !初始化
     call init_assembly(assm1)
     !    timer1%init(ttotal,Nt,ctime,ltime)
     call timer1%init(0.0,0.0)
    end subroutine Sys_pre_process
    
         subroutine init_assembly(assm)
      implicit none
      type(sys_assembly),intent(in out)::assm
      write(*,*)'init assembly'
      !热物性初始化
      call assm%property%init(assm%mesh%Nf,assm%mesh%Ng,assm%mesh%Ns,assm%mesh%Ny)
      !热工参数初始化
      call assm%Thermal%init(assm%initdata%Ti,assm%initdata%Pi,assm%initdata%ui)
      !边界条件初始化
      call assm%th_boundary%init(assm%initdata%Tin,assm%initdata%uin,assm%initdata%pout)
      assm%th_boundary%p%inlet=assm%thermal%pressure(1)+2500.0
      !热源初始化
      !write(*,*)'init power'
      !assm%pow%power=0.0
      !assm%pow%fq_core=0.0
      !网格
      call cal_grid(assm)
      call assm%hydrau%cal(assm%geom%pellet,assm%geom%pd)
     endsubroutine init_assembly
    
     subroutine alloc_assembly(assm)
      implicit none
      type(sys_assembly),intent(in out)::assm
      !local
      integer::i_allocate
      integer::M,N,Ny
      Ny=assm%mesh%Ny
      M=Ny+1
      N=assm%mesh%Nf+assm%mesh%Ng+assm%mesh%Ns+1
      !check allocated first
      call Free_assembly(assm)
      
      allocate(assm%property%rho(0:M,0:N))!(1:M,1:N)
      allocate(assm%property%shc(0:M,0:N))
      allocate(assm%property%ctc(0:M,0:N))
	  allocate(assm%property%dvs(0:M,0:N))
      allocate(assm%property%htc(0:M))
      
      allocate(assm%thermal%Temperature(M-1,N))
      allocate(assm%thermal%Pressure(M-1))
      allocate(assm%thermal%Velocity(Ny-1))
      
      allocate(assm%mesh%r(0:M,0:N))
      allocate(assm%mesh%z(0:M,0:N))
      
      allocate(assm%pow%power(M-1,N))
      allocate(assm%pow%fq_core(M-1,N))
      write(*,*)'alloc data array'
     end subroutine alloc_assembly
     
     subroutine Free_assembly(assm)
      implicit none
      type(sys_assembly),intent(in out)::assm
      if(allocated(assm%property%rho))  deallocate(assm%property%rho)
      if(allocated(assm%property%shc))  deallocate(assm%property%shc)
      if(allocated(assm%property%ctc))  deallocate(assm%property%ctc)
      if(allocated(assm%property%htc))  deallocate(assm%property%htc)
      
      if(allocated(assm%thermal%temperature))  deallocate(assm%thermal%temperature)
      if(allocated(assm%thermal%pressure))  deallocate(assm%thermal%pressure)
      if(allocated(assm%thermal%Velocity))  deallocate(assm%thermal%Velocity)
      
      if(allocated(assm%mesh%r))  deallocate(assm%mesh%r)
      if(allocated(assm%mesh%z))  deallocate(assm%mesh%z)
      
      if(allocated(assm%pow%power))  deallocate(assm%pow%power)
      if(allocated(assm%pow%fq_core))  deallocate(assm%pow%fq_core)
     end subroutine Free_assembly
     
     subroutine set_assembly(assm,reInputdata)
      implicit none
      type(sys_assembly),intent(in out)::assm
      type(sys_re_input),intent(in)::reInputdata
      write(*,*)'set assmebly as below:'
      !设置几何参数
      call assm%geom%set(reInputdata%xf,reInputdata%xg,reInputdata%xs,reInputdata%acf,reInputdata%Height,reInputdata%pd,reInputdata%npin)
      !设置网格参数
      call assm%mesh%set(reInputdata%ny,reInputdata%ny_start,reInputdata%ny_end)
      !设置初始值
      call assm%initdata%set(reInputdata%Ti,reInputdata%Pi,reInputdata%Ui,reInputdata%Tin,reInputdata%Pin,reInputdata%Uin)
      !设置收敛因子
      call assm%confactor_%set(reInputdata%alpha,reInputdata%sigma)
      call assm%hydrau%set(reInputdata%f)
     end subroutine set_assembly
     
     subroutine cal_grid(assm)
       implicit none
       type(sys_assembly),intent(in out)::assm
       !local
       real Df,Dg,Ds,Dy 
       integer  M,N,i,j
     write(*,*)'calculate the grid value...'
     Df=assm%geom%pellet/assm%mesh%Nf
     Dg=assm%geom%Bond/assm%mesh%Ng
     Ds=assm%geom%Cladth/assm%mesh%Ns
     Dy=assm%geom%Height/assm%mesh%Ny
     M=assm%mesh%Ny+1
     N=assm%mesh%Nf+assm%mesh%Ng+assm%mesh%Ns+1
     
     Do i=0,M,1
         do j=0,N,1
            if (j==0)then
               assm%mesh%r(i,j)=0.0
            elseif(j==1)then
               assm%mesh%r(i,j)=Df/2.0
            elseif(j>1.and.j<=assm%mesh%Nf) then
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+Df
            elseif(j==assm%mesh%Nf+1)then
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+Df/2.0+Dg/2.0
            elseif (j>assm%mesh%Nf+1.and.j<=assm%mesh%Nf+assm%mesh%Ng) then
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+Dg
            elseif (j==assm%mesh%Nf+assm%mesh%Ng+1)then
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+ Dg/2.0+Ds/2.0
            elseif (j>assm%mesh%Nf+assm%mesh%Ng+1.and.j<=assm%mesh%Nf+assm%mesh%Ng+assm%mesh%Ns)then
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+Ds
            else!流体的径向坐标，没有实际意义
               assm%mesh%r(i,j)=assm%mesh%r(i,j-1)+Ds
            endif
            
            if(i==0)then
              assm%mesh%z(i,j)=0.0
            elseif (i==1)then
              assm%mesh%z(i,j)=Dy/2.0
            elseif (i>1.and.i<M)then
              assm%mesh%z(i,j)=assm%mesh%z(i-1,j)+Dy
            elseif (i==M)then
              assm%mesh%z(i,j)=assm%mesh%z(i-1,j)+Dy/2.0
            endif
         enddo
      enddo   
     end subroutine cal_grid   

end module imp_driving_pre_process