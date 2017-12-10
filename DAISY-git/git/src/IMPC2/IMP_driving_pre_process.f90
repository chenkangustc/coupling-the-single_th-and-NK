module imp_driving_pre_process
    use constants
	use global 
    use global_state
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
     real(KREAL)::ttotal
     real(KREAL)::ltime
     real(KREAL)::ctime
     integer::Nt
     write(*,*)'start the sys pre process:'
     !��ȡ����
     call reInputdata%set()
     !call reInputdata%publish()
     !������ֵ
     call set_assembly(assm1,reInputdata,ns%state%zone,ns%state%layer,ns%state%layer_bottom,ns%state%layer_top)
	 print*,'set nf ng ns...'

	 print*,'nf=',assm1%mesh%nf,'ng=',assm1%mesh%ng,'ns=',assm1%mesh%ns
     ttotal=150.0
     Nt=150
     call timer1%set(ttotal,Nt)!(ttotal,Nt)
     !����ռ�
     call alloc_assembly(assm1)
     call timer1%alloc()
     !��ʼ��
     call init_assembly(assm1)
     !    timer1%init(ttotal,Nt,ctime,ltime)
     ltime=0.0
     ctime=0.0
     call timer1%init(ctime,ltime)
    end subroutine Sys_pre_process
    
         subroutine init_assembly(assm)
      implicit none
      type(sys_assembly),intent(in out)::assm
      write(*,*)'init assembly'
      !�����Գ�ʼ��
      call assm%property%init(assm%mesh%Nf,assm%mesh%Ng,assm%mesh%Ns,assm%mesh%Ny)
      !�ȹ�������ʼ��
      call assm%Thermal%init(assm%initdata%Ti,assm%initdata%Pi,assm%initdata%ui)
      !�߽�������ʼ��
      call assm%th_boundary%init(assm%initdata%Tin,assm%initdata%uin,assm%initdata%pout)
      assm%th_boundary%p%inlet=assm%thermal%pressure(1)+2500.0
      !��Դ��ʼ��
      !write(*,*)'init power'
      !assm%pow%power=0.0
      !assm%pow%fq_core=0.0
      !����
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
     
     subroutine set_assembly(assm,reInputdata,zone,layer,layer_start,layer_end)
      implicit none
      type(sys_assembly),intent(in out)::assm
      type(sys_re_input),intent(in)::reInputdata
	  integer,intent(in)::zone
	  integer,intent(in)::layer
	  integer,intent(in)::layer_start
	  integer,intent(in)::layer_end
	  !local
	  integer n_start
	  integer n_end
	  n_start=layer_start+1
	  n_end=layer-layer_end
	  !real,intent(in)::height(:)
      write(*,*)'set assmebly as below:'
      !���ü��β���
      call assm%geom%set(reInputdata%xf,reInputdata%xg,reInputdata%xs,reInputdata%acf,reInputdata%Height,reInputdata%pd,reInputdata%npin)
      !�����������
      call assm%mesh%set(reInputdata%nf,reInputdata%ng,reInputdata%ns,zone,layer,n_start,n_end)
      !���ó�ʼֵ
      call assm%initdata%set(reInputdata%Ti,reInputdata%Pi,reInputdata%Ui,reInputdata%Tin,reInputdata%Pin,reInputdata%Uin)
      !������������
      call assm%confactor_%set(reInputdata%alpha,reInputdata%sigma)
      call assm%hydrau%set(reInputdata%f)
     end subroutine set_assembly
     
     subroutine cal_grid(assm)
       implicit none
       type(sys_assembly),intent(in out)::assm
       !local
       real(KREAL):: Df,Dg,Ds,Dy 
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
            else!����ľ������꣬û��ʵ������
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