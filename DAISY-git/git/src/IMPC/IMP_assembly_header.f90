module IMP_assembly_header
    use IMP_assm_header
    use IMP_re_input_global
    use IMP_driving_cal_transient
    use IMP_mathkerel
    
    implicit none
    type,public::sys_assembly!描述一个组件的特征和使用方法
      !private
      !real::fric  !摩擦因子
      type(hydraulic)::hydrau
      type(AssmGeom)::geom !assm_geom
      type(AssmMesh)::mesh !Assm_mesh
      type(material)::property !Assm_material 热物性和水力学参数
      type(th_boundary)::th_boundary !Assm_th_boundary
      type(AssmInit)::initdata
      type(confactor)::confactor_
      type(assmpow)::pow
      !real,allocatable::power(:,:) !Assm_power(zone,layer)
      !real,allocatable::fq_core(:,:)
      type(thermal)::Thermal  !pvt
    contains
      procedure,public::alloc=>alloc_assembly
      procedure,public::clean=>free_assembly
      procedure,public::set=>set_assembly!输入卡输入
      procedure,public::init=>init_assembly
      procedure,public::grid=>cal_grid
      procedure,public::Steady=>cal_Assembly_Steady
      procedure,public::Transient=>cal_Assembly_Transient
      
      procedure,public::solve_momentum
      procedure,public::cal_momentumA
     !procedure,public::solve_momentumA
      procedure,public::solve_pressureCorrection
      procedure,public::cal_pmodifyA
     !procedure,public::solve_pmodifyA
      procedure,public::modify_PV
      procedure,public::cal_th_convection
      procedure,public::cal_th_temperature
      procedure,public::solve_temperature
      procedure,public::update_property
    end type sys_assembly
     private::alloc_assembly
     private::free_assembly
     private::set_assembly
     private::init_assembly
     private::cal_Assembly_Steady
     private::cal_Assembly_Transient
     
     private::solve_momentum
     private::cal_momentumA
     !private::solve_momentumA
     private::solve_pressureCorrection
     private::cal_pmodifyA
     !private::solve_pmodifyA
     private::modify_PV
     private::cal_th_convection
     private::cal_th_temperature
     private::solve_temperature
     private::update_property
     
    contains
     subroutine init_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      write(*,*)'init assembly'
      !热物性初始化
      call this%property%init(this%mesh%Nf,this%mesh%Ng,this%mesh%Ns,this%mesh%Ny)
      !热工参数初始化
      call this%Thermal%init(this%initdata%Ti,this%initdata%Pi,this%initdata%ui)
      !边界条件初始化
      call this%th_boundary%init(this%initdata%Tin,this%initdata%uin,this%initdata%pout)
      this%th_boundary%p%inlet=this%thermal%pressure(1)+2500.0
      !热源初始化
      !write(*,*)'init power'
      !this%pow%power=0.0
      !this%pow%fq_core=0.0
      !网格
      call this%grid()
      call this%hydrau%cal(this%geom%rFuel,this%geom%pd)
     endsubroutine init_assembly
    
     subroutine alloc_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      !local
      integer::i_allocate
      integer::M,N,Ny
      Ny=this%mesh%Ny
      M=Ny+1
      N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
      !check allocated first
      call this%clean()
      
      allocate(this%property%rho(0:M,0:N))!(1:M,1:N)
      allocate(this%property%shc(0:M,0:N))
      allocate(this%property%ctc(0:M,0:N))
	  allocate(this%property%dvs(0:M,0:N))
      allocate(this%property%htc(0:M))
      
      allocate(this%thermal%Temperature(M-1,N))
      allocate(this%thermal%Pressure(M-1))
      allocate(this%thermal%Velocity(Ny-1))
      
      allocate(this%mesh%r(0:M,0:N))
      allocate(this%mesh%z(0:M,0:N))
      
      allocate(this%pow%power(M-1,N))
      allocate(this%pow%fq_core(M-1,N))
      write(*,*)'alloc data array'
     end subroutine alloc_assembly
     
     subroutine Free_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      if(allocated(this%property%rho))  deallocate(this%property%rho)
      if(allocated(this%property%shc))  deallocate(this%property%shc)
      if(allocated(this%property%ctc))  deallocate(this%property%ctc)
      if(allocated(this%property%htc))  deallocate(this%property%htc)
      
      if(allocated(this%thermal%temperature))  deallocate(this%thermal%temperature)
      if(allocated(this%thermal%pressure))  deallocate(this%thermal%pressure)
      if(allocated(this%thermal%Velocity))  deallocate(this%thermal%Velocity)
      
      if(allocated(this%mesh%r))  deallocate(this%mesh%r)
      if(allocated(this%mesh%z))  deallocate(this%mesh%z)
      
      if(allocated(this%pow%power))  deallocate(this%pow%power)
      if(allocated(this%pow%fq_core))  deallocate(this%pow%fq_core)
     end subroutine Free_assembly
     
     subroutine set_assembly(this,reInputdata)
      implicit none
      class(sys_assembly),intent(in out)::this
      type(sys_re_input),intent(in)::reInputdata
      write(*,*)'set assmebly as below:'
      !设置几何参数
      call this%geom%set(reInputdata%xf,reInputdata%xg,reInputdata%xs,reInputdata%xos,reInputdata%acf,reInputdata%Height,reInputdata%pd,reInputdata%npin)
      !设置网格参数
      call this%mesh%set(reInputdata%nf,reInputdata%ng,reInputdata%ns,reInputdata%ny)
      !设置初始值
      call this%initdata%set(reInputdata%Ti,reInputdata%Pi,reInputdata%Ui,reInputdata%Tin,reInputdata%Pin,reInputdata%Uin)
      !设置收敛因子
      call this%confactor_%set(reInputdata%alpha,reInputdata%sigma)
      call this%hydrau%set(reInputdata%f)
     end subroutine set_assembly
     
     subroutine cal_grid(this)
       implicit none
       class(sys_assembly),intent(in out)::this
       !local
       real Df,Dg,Ds,Dy 
       integer  M,N,i,j
     write(*,*)'calculate the grid value...'
     Df=this%geom%rFuel/this%mesh%Nf
     Dg=this%geom%GasGap/this%mesh%Ng
     Ds=this%geom%ShellThick/this%mesh%Ns
     Dy=this%geom%Height/this%mesh%Ny
     M=this%mesh%Ny+1
     N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
     
     Do i=0,M,1
         do j=0,N,1
            if (j==0)then
               this%mesh%r(i,j)=0.0
            elseif(j==1)then
               this%mesh%r(i,j)=Df/2.0
            elseif(j>1.and.j<=this%mesh%Nf) then
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+Df
            elseif(j==this%mesh%Nf+1)then
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+Df/2.0+Dg/2.0
            elseif (j>this%mesh%Nf+1.and.j<=this%mesh%Nf+this%mesh%Ng) then
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+Dg
            elseif (j==this%mesh%Nf+this%mesh%Ng+1)then
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+ Dg/2.0+Ds/2.0
            elseif (j>this%mesh%Nf+this%mesh%Ng+1.and.j<=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns)then
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+Ds
            else!流体的径向坐标，没有实际意义
               this%mesh%r(i,j)=this%mesh%r(i,j-1)+Ds
            endif
            
            if(i==0)then
              this%mesh%z(i,j)=0.0
            elseif (i==1)then
              this%mesh%z(i,j)=Dy/2.0
            elseif (i>1.and.i<M)then
              this%mesh%z(i,j)=this%mesh%z(i-1,j)+Dy
            elseif (i==M)then
              this%mesh%z(i,j)=this%mesh%z(i-1,j)+Dy/2.0
            endif
         enddo
      enddo   
     end subroutine cal_grid
     
subroutine cal_Assembly_Steady(this,power,fq_core)
    class(sys_assembly),intent(in out)::this
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
    Ny=this%mesh%Ny
    M=Ny+1
    N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
    allocate(pmodify(Ny),ui(Ny-1),Ti(M-1,N),rhoi(0:M,0:N),rhofi(0:M),ap(Ny-1))
 
    pmodify=0.0
    ui=this%thermal%velocity
    Ti=this%thermal%temperature
    rhoi=this%property%rho
    rhofi=this%property%rho(:,N)
    ap=0.0
    j=0
    drho=1.0
    call this%pow%set(power,fq_core)
    do while(drho>this%confactor_%alpha)  
       i=0
       btotal=1.0
      do while(btotal>this%confactor_%sigma)
	   i=i+1
       call solve_momentum(this,flag,rhofi,ui,dt,ap)!需要输入当前迭代步的rho,uz 
       call solve_pressureCorrection(this,flag,ap,rhofi,dt,pmodify,btotal)
       call modify_PV(this,ap,pmodify)
       print*,'pv step=',i,' btotal=',btotal
      end do
      j=j+1
      call solve_temperature(this,flag,Ti,rhoi,dt)
      call update_property(this,drho)!物性更新
      print*,'property step=',j,' drho=',drho
    end do
  end subroutine cal_Assembly_Steady
     
subroutine cal_Assembly_Transient(this,power,fq_core,ltime,ctime)
    class(sys_assembly),intent(in out)::this
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
    Ny=this%mesh%Ny
    M=Ny+1
    N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
    allocate(pmodify(Ny),ui(Ny-1),Ti(M-1,N),rhoi(0:M,0:N),rhofi(0:M),ap(Ny-1))
    
    pmodify=0.0
    Ti=this%thermal%temperature
    ui=this%thermal%velocity
    rhoi=this%property%rho
    rhofi=this%property%rho(:,N)
    ap=0.0
    call this%th_boundary%update(ctime)
    call this%pow%set(power,fq_core)
    j=0
    drho=1.0
    do while(drho>this%confactor_%alpha)
       i=0
       btotal=1.0
      do while(btotal>this%confactor_%sigma)
       i=i+1
       call solve_momentum(this,flag,rhofi,ui,dt,ap)!需要输入当前迭代步的rho,uz 
       call solve_pressureCorrection(this,flag,ap,rhofi,dt,pmodify,btotal)
       call modify_PV(this,ap,pmodify)
       print*,'pv step=',i,' btotal=',btotal
      end do
      j=j+1     
      call solve_temperature(this,flag,Ti,rhoi,dt)
      call update_property(this,drho)!物性更新
      print*,'density step=',j,' drho=',drho
    end do
  end subroutine cal_Assembly_Transient

subroutine solve_momentum(this,flag,rhoi,ui,dt,ap)!(几何网格水力学)（p）（初始条件）（边界条件）（瞬态计算选项）
        class(sys_assembly),intent(in out)::this
        real,intent(in)::flag
        !real,intent(in)::pguess(:)
        real,intent(in)::rhoi(:)
        real,intent(in)::ui(:)
        real,intent(in)::dt
        real,intent(in out)::ap(:)
        !local
        integer M!M=Ny-1,矢量控制体的个数
        real,allocatable::A(:,:),b(:)
        M=this%mesh%ny-1
        allocate(A(1:M,1:M),b(1:M))
        !pguess=this%thermal%pressure
        call cal_momentumA(this,flag,rhoi,ui,dt,A,b,ap)
        call solve_momentumA(M,A,b,this%thermal%Velocity)

    end subroutine solve_momentum

    !subroutine cal_momentumA(N,f,De,rhoi,rho,uin,ui,ulast,pin,pout,pguess,dx,dt,A,b)!rho是当前迭代步的物性
subroutine cal_momentumA(this,flag,rhoi,ui,dt,A,b,ap)
     implicit none
     class(sys_assembly),intent(in out)::this
     real,intent(in)::flag
     !real,intent(in)::pguess(:)
     real,intent(in)::rhoi(:),ui(:)
     real,intent(in)::dt
     real,intent(in out)::A(:,:)
     real,intent(in out)::b(:)
     real,intent(in out)::ap(:)
      !local
       integer N,i!N是矢量控制体的个数
       integer nr,nz!nr是径向的控制体个数，nz是轴向的标量控制体个数
       real dx
       real f,De
       real uin,pin,pout
       real,allocatable::ulast(:),pguess(:)
       real,allocatable::rho(:)
       real,allocatable::aw(:),ae(:),api(:),bs(:)
       N=this%mesh%ny-1
       nr=this%mesh%nf+this%mesh%ng+this%mesh%ns
       nz=this%mesh%ny
       allocate(aw(1:N),ae(1:N),api(1:N),bs(1:N))
       allocate(rho(0:nz+1),ulast(1:N),pguess(1:N+1))
        dx=this%geom%height/this%mesh%Ny
        f=this%hydrau%fric
        de=this%hydrau%de
        rho=this%property%rho(:,nr+1)
        uin=this%th_boundary%u%inlet
        pin=this%th_boundary%p%inlet
        pout=this%th_boundary%p%outlet
        ulast=this%thermal%velocity
        pguess=this%thermal%Pressure
       !dx rhoi,ulast,f,De,rho,uin,pin,api,pout
       !计算各个控制体的常系数和源项
       api=0.0
       do i=1,N,1
         if(i==1)then
             aw(i)=0.0
             ae(i)=0.0
             if (flag==1.0) api(i)=1.50*dx/dt*(2*RHOI(i)+RHOI(i+1))/3.0
             ap(i)=api(i)+f/(2*De)*1.50*dx*ulast(i)*(2*RHO(i)+RHO(i+1))/3.0+RHO(0)*uin!replace rho(0)*uin with rho(1)*uin
             bs(i)=pin-pguess(i+1)-(2*RHO(i)+RHO(i+1))/3.0*9.8*1.5*dx+RHO(0)*uin*uin  !replace rho(0)*uin with rho(1)*uin           
         elseif(i>1.and.i<N)then
             aw(i)=ulast(i-1)*RHO(i)
             ae(i)=0.0
             if(flag==1.0) api(i)=dx/dt*(RHOI(i)+RHOI(i+1))/2.0
             ap(i)=aw(i)+api(i)+f/(2*De)*dx*ulast(i)*(RHO(i)+RHO(i+1))/2.0
             bs(i)=pguess(i)-pguess(i+1)-(RHO(i)+RHO(i+1))/2.0*9.8*dx
         elseif(i==N)then
             aw(i)=RHO(N)*ulast(N-1)
             ae(i)=0.0
             if(flag==1.0) api(i)=1.50*dx/dt*(RHOI(N)+2*RHOI(N+1))/3.0
             ap(i)=aw(i)+api(i)+f/(2*De)*1.50*dx*ulast(N)*(RHO(N)+2*RHO(N+1))/3.0
             bs(i)=pguess(i)-pout-(RHO(i)+2*RHO(i+1))/3.0*9.8*1.5*dx            
         endif
       enddo
       
      !用直接发求矩阵，第一步，写出系数矩阵
      A=0.0
       do i=1,N,1
           if(i==1)then
               A(i,i)=ap(i)
           elseif(i>1.and.i<N)then
               A(i,i)=ap(i)
               A(i,i-1)=-aw(i) 
           elseif(i==N)then
               A(i,i)=ap(i)
               A(i,i-1)=-aw(i)
           endif
           b(i)=bs(i)+api(i)*ui(i)
       enddo
    end subroutine cal_momentumA
    

    
subroutine solve_pressureCorrection(this,flag,ap,rhoi,dt,pmodify,btotal)
    implicit none
    class(sys_assembly),intent(in out)::this
    real,intent(in)::flag
    real,intent(in)::ap(:)
    real,intent(in)::rhoi(:)
    real,intent(in)::dt
    real,intent(in out)::pmodify(:)
    real,intent(in out)::btotal
    !local
    integer N
    real,allocatable::A(:,:),b(:)
    N=size(pmodify)
    allocate(A(N-1,N-1),b(N-1))
    call cal_pmodifyA(this,flag,ap,rhoi,dt,A,b,btotal)
    call solve_pmodifyA(A,b,pmodify)
end subroutine solve_pressureCorrection
    
subroutine cal_pmodifyA(this,flag,ap,rhoi,dt,A,b,btotal)
 implicit none
 class(sys_assembly),intent(in out)::this
 real,intent(in)::flag
 real,intent(in)::ap(:)
 real,intent(in)::rhoi(:)
 real,intent(in)::dt
 real,intent(in out)::A(:,:)
 real,intent(in out)::b(:)
 real,intent(in out)::btotal
 !local
 integer Ny,i,nr
 real uin,pout,dx
 real,allocatable::rho(:),ulast(:)
 real,allocatable::bp(:),be(:),bw(:),bb(:)

 Ny=this%mesh%Ny
 nr=this%mesh%nf+this%mesh%ng+this%mesh%ns
 allocate(RHO(0:Ny+1),ulast(1:Ny-1))
 allocate(bp(1:Ny),be(1:Ny),bw(1:Ny),bb(1:Ny))

 uin=this%th_boundary%u%inlet
 pout=this%th_boundary%p%outlet
 dx=this%geom%height/this%mesh%Ny
 rho=this%property%rho(:,nr+1)
 ulast=this%thermal%velocity
        if(flag==0.0) then !steady
            dx=0.0
        endif
 
        do i=1,Ny,1
           if(i==1)then
             be(i)=1.5*(RHO(i)+RHO(i+1))/2.0/ap(i)
             bw(i)=0.0
             bp(i)=be(i)
             bb(i)=RHO(0)*uin-(RHO(i)+RHO(i+1))/2.0*ulast(i)+(RHOI(i)-RHO(i))*dx/dt
           elseif(i==2)then
             be(i)=(RHO(i)+RHO(i+1))/2.0/ap(i)
             bw(i)=1.5*(RHO(i)+RHO(i-1))/2.0/ap(i-1)
             bp(i)=be(i)+bw(i)
             bb(i)=(RHO(i)+RHO(i-1))/2.0*ulast(i-1)-(RHO(i)+RHO(i+1))/2.0*ulast(i)+(RHOI(i)-RHO(i))*dx/dt               
           elseif(i>2.and.i<Ny-1)then
             be(i)=(RHO(i)+RHO(i+1))/2.0/ap(i)
             bw(i)=(RHO(i)+RHO(i-1))/2.0/ap(i-1)
             bp(i)=be(i)+bw(i)
             bb(i)=(RHO(i)+RHO(i-1))/2.0*ulast(i-1)-(RHO(i)+RHO(i+1))/2.0*ulast(i)+(RHOI(i)-RHO(i))*dx/dt    
           elseif(i==Ny-1)then
             be(i)=0.0
             bw(i)=(RHO(i)+RHO(i-1))/2.0/ap(i-1)
             bp(i)=bw(i)+(RHO(i)+RHO(i+1))/2.0/ap(i)
             bb(i)=(RHO(i)+RHO(i-1))/2.0*ulast(i-1)-(RHO(i)+RHO(i+1))/2.0*ulast(i)+(RHOI(i)-RHO(i))*dx/dt  
           elseif(i==Ny)then
             be(i)=0.0
             bw(i)=0.0
             bp(i)=0.0
             bb(i)=0.0
           endif
       enddo
       
       A=0.0
       do i=1,Ny-1,1
           if(i==1)then
               A(i,i)=bp(i)
               A(i,i+1)=-be(i)
               b(i)=bb(i)
           elseif(i>1.and.i<Ny-1)then
               A(i,i)=bp(i)
               A(i,i+1)=-be(i)
               A(i,i-1)=-bw(i)
               b(i)=bb(i)
           elseif(i==Ny-1)then
               A(i,i)=bp(i)
               A(i,i-1)=-bw(i)
               b(i)=bb(i)
           endif
       enddo
       
      btotal=0.0
      do i=1,Ny,1
        btotal=btotal+abs(bb(i))
      enddo
end subroutine cal_pmodifyA



subroutine modify_PV(this,ap,pmodify)
  implicit none
  class(sys_assembly),intent(in out)::this
  real,intent(in)::ap(:)
  real,intent(in out)::pmodify(:)
  !local
  integer i,Ny
  real alpha
  Ny=this%mesh%Ny
  alpha=0.8
            do i=1,Ny-1,1
                 this%thermal%pressure(i)=this%thermal%pressure(i)+alpha*pmodify(i)
               if(i==1)then
                 this%thermal%velocity(i)=this%thermal%velocity(i)+1.5*(pmodify(i)-pmodify(i+1))/ap(i)
               elseif(i>1.and.i<Ny-1)then
                 this%thermal%velocity(i)=this%thermal%velocity(i)+(pmodify(i)-pmodify(i+1))/ap(i)
               elseif(i==Ny-1)then
               !this%thermal%velocity(i)=this%thermal%velocity(i)+(pmodify(i)-pout)/ap(i)!pmout=0.0
                 this%thermal%velocity(i)=this%thermal%velocity(i)+pmodify(i)/ap(i)
               endif
            enddo
            this%th_boundary%u%outlet=this%thermal%velocity(Ny-1)
            this%th_boundary%p%inlet=1.50*this%thermal%pressure(1)-0.5*this%thermal%pressure(2)
            this%thermal%pressure(Ny)=(2*this%th_boundary%p%outlet+this%thermal%pressure(Ny-1))/3.0
end subroutine modify_PV

subroutine cal_th_convection(this)
 implicit none
 class(sys_assembly),intent(in out)::this
 !lcoal
 integer i,Ny,nr
 real De,Area,wet,velocity!单纯输入的变量可以用局部变量来替换
 real,allocatable::rho(:),dvs(:),shc(:),ctc(:)
 Ny=this%mesh%Ny
 nr=this%mesh%nf+this%mesh%ng+this%mesh%ns
 allocate(rho(0:Ny+1),dvs(0:Ny+1),shc(0:Ny+1),ctc(0:Ny+1))
 de=this%hydrau%de
 Area=this%hydrau%aflow
 wet=this%hydrau%wet
 rho=this%property%rho(:,nr+1)
 dvs=this%property%dvs(:,nr+1)
 shc=this%property%shc(:,nr+1)
 ctc=this%property%ctc(:,nr+1)
 
     do i=1,Ny,1
      if (i==1)then
          velocity=this%thermal%velocity(1)
      elseif(i>1.and.i<Ny)then
          velocity=(this%thermal%velocity(i-1)+this%thermal%velocity(i))/2.0
      else
          velocity=this%thermal%velocity(i-1)
      endif
      call get_convection(De,Area,wet,RHO(i),velocity,DVS(i),SHC(i),CTC(i),this%property%htc(i))!DVS(i,N)动力粘度 Pa*s
    enddo
    this%property%htc(0)=this%property%htc(1)!边界上的对流换热系数
    this%property%htc(Ny+1)=this%property%htc(Ny)
end subroutine cal_th_convection
    
subroutine cal_th_temperature(this,flag,Ti,rhoi,dt)
 implicit none
 class(sys_assembly),intent(in out)::this
 real,intent(in)::flag
 real,intent(in)::Ti(:,:),rhoi(:,:)
 real,intent(in)::dt
 !local
    real Area,Xt,xf,xg,xs,Df,Dg,Ds,Dy,uin,Tin
    integer  M,N,i,j,k,Nf,Ng,Ns,Ny
    real,allocatable::Tj(:,:)
    real,allocatable::RHO(:,:),SHC(:,:),CTC(:,:),DVS(:,:)
    real,allocatable::aw(:,:),ae(:,:),ap(:,:),as(:,:),an(:,:),api(:,:),bs(:,:),q(:,:)
     Area=this%hydrau%aflow
     uin=this%th_boundary%u%inlet
     Tin=this%th_boundary%T%inlet
     xf=this%geom%rFuel
     xg=this%geom%GasGap
     xs=this%geom%ShellThick
     Xt=Xf+Xg+Xs !包壳外径
     Nf=this%mesh%Nf
     Ng=this%mesh%Ng
     Ns=this%mesh%Ns
     Ny=this%mesh%Ny
     M=this%mesh%Ny+1
     N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
     Df=Xf/Nf
     Dg=Xg/Ng
     Ds=Xs/Ns
     Dy=this%geom%height/Ny
     
    allocate(Tj(1:M-1,1:N))
    allocate(RHO(0:M,0:N),SHC(0:M,0:N),CTC(0:M,0:N),DVS(0:M,0:N))
    allocate(aw(1:M-1,1:N),ae(1:M-1,1:N),ap(1:M-1,1:N),as(1:M-1,1:N),an(1:M-1,1:N),api(1:M-1,1:N),bs(1:M-1,1:N),q(1:M-1,1:N))
    rho=this%property%rho
    shc=this%property%shc
    ctc=this%property%ctc
    dvs=this%property%dvs
    !set the pow
    q=this%pow%power
    
    api=0.0
    Do i=1,M-1,1
        Do j=1,N,1
         if (j==1)then!轴对称边界的控制体
          aw(i,j)=0.0
          ae(i,j)=(this%mesh%r(i,j)+Df/2.0)*CTC(i,j)/Df
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Df/dt
          ap(i,j)=ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Df*q(i,j)
         elseif (j>1.and.j<Nf)then!fuel内部控制体
          aw(i,j)=(this%mesh%r(i,j)-Df/2.0)*CTC(i,j)/Df
          ae(i,j)=(this%mesh%r(i,j)+Df/2.0)*CTC(i,j)/Df
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Df/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Df*q(i,j)
         elseif(j==Nf)then!f-g边界左侧控制体
          aw(i,j)=(this%mesh%r(i,j)-Df/2.0)*CTC(i,j)/Df
          ae(i,j)=2*(this%mesh%r(i,j)+Df/2.0)*(Df+Dg)/(Df/CTC(i,j)+Dg/CTC(i,j+1))/(Df+Dg)
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Df/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Df*q(i,j)  
         elseif (j==Nf+1)then!f-g边界右侧控制体
          aw(i,j)=2*(this%mesh%r(i,j)-Dg/2.0)*(df+dg)/(df/CTC(i,j-1)+dg/CTC(i,j))/(df+dg)
          ae(i,j)=(this%mesh%r(i,j)+Dg/2.0)*CTC(i,j)/Dg
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Dg/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Dg*q(i,j)
         elseif(j>Nf+1.and.j<Nf+Ng)then!g气隙内部控制体
          aw(i,j)=(this%mesh%r(i,j)-Dg/2.0)*CTC(i,j)/Dg
          ae(i,j)=(this%mesh%r(i,j)+Dg/2.0)*CTC(i,j)/Dg
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Dg/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Dg*q(i,j)
         elseif(j==Nf+Ng)then!g-c边界左侧控制体
          aw(i,j)=(this%mesh%r(i,j)-Dg/2.0)*CTC(i,j)/Dg
          ae(i,j)=2*(this%mesh%r(i,j)+Dg/2.0)*(Dg+Ds)/(Dg/CTC(i,j)+Ds/CTC(i,j+1))/(Dg+Ds)
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Dg/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Dg*q(i,j)
         elseif(j==Nf+Ng+1)then!g-c边界右侧控制体
          aw(i,j)=2*(this%mesh%r(i,j)-Ds/2.0)*(dg+ds)/(dg/CTC(i,j-1)+ds/CTC(i,j))/(dg+ds)
          ae(i,j)=(this%mesh%r(i,j)+Ds/2.0)*CTC(i,j)/Ds
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Ds/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Ds*q(i,j)
         elseif(j>Nf+Ng+1.and.j<Nf+Ng+Ns)then!c包壳内部控制体
          aw(i,j)=(this%mesh%r(i,j)-Ds/2.0)*CTC(i,j)/Ds
          ae(i,j)=(this%mesh%r(i,j)+Ds/2.0)*CTC(i,j)/Ds
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Ds/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Ds*q(i,j)
         elseif(j==Nf+Ng+Ns)then!s-fluid边界左侧控制体
          aw(i,j)=(this%mesh%r(i,j)-Ds/2.0)*CTC(i,j)/Ds
          ae(i,j)=(this%mesh%r(i,j)+Ds/2.0)/(1.0/this%property%htc(i)+ds/(2.0*CTC(i,j)))
          as(i,j)=0.0
          an(i,j)=0.0
          if(flag==1.0) api(i,j)=RHO(i,j)*SHC(i,j)*this%mesh%r(i,j)*Ds/dt
          ap(i,j)=aw(i,j)+ae(i,j)+api(i,j)
          bs(i,j)=this%mesh%r(i,j)*Ds*q(i,j)
         elseif(j==Nf+Ng+Ns+1)then!fluid控制体
          
          if(i==1)then!流体入口的控制体
           aw(i,j)=Dy/SHC(i,j)*2.0*3.14*Xt/Area*1.0/(1.0/this%property%htc(i)+Ds/(2*CTC(i,j-1)))
           ae(i,j)=0.0
           as(i,j)=0.0
           an(i,j)=0.0
           if(flag==1.0) api(i,j)=RHOI(i,j)*Dy/dt
           ap(i,j)=aw(i,j)+api(i,j)+RHO(i-1,j)*uin
           bs(i,j)=Dy/SHC(i,j)*q(i,j)+RHO(i-1,j)*uin*this%th_boundary%T%inlet          
          else!流体内部以及出口控制体
           aw(i,j)=Dy/SHC(i,j)*2.0*3.14*Xt/Area*1.0/(1.0/this%property%htc(i)+Ds/(2*CTC(i,j-1)))
           ae(i,j)=0.0
           as(i,j)=0.0
           an(i,j)=0.5*(RHO(i,j)+RHO(i-1,j))*this%thermal%velocity(i-1)
           if(flag==1.0) api(i,j)=RHOI(i,j)*Dy/dt
           ap(i,j)=an(i,j)+aw(i,j)+api(i,j)
           bs(i,j)=Dy/SHC(i,j)*q(i,j)        
          endif
         endif              
      enddo
    enddo

     
     do k=1,100,1
       do i=1,M-1,1
           do j=1,N,1
               if(k==1) then
                Tj(i,j)=Ti(i,j)
               else
                if(j==1)then
                 Tj(i,j)=(ae(i,j)*Tj(i,j+1)+bs(i,j)+api(i,j)*Ti(i,j))/ap(i,j)
                elseif(j>1.and.j<N)then
                 Tj(i,j)=(aw(i,j)*Tj(i,j-1)+ae(i,j)*Tj(i,j+1)+bs(i,j)+api(i,j)*Ti(i,j))/ap(i,j)
                elseif(j==N) then
                  if(i==1)then
                    Tj(i,j)=(aw(i,j)*Tj(i,j-1)+bs(i,j)+api(i,j)*Ti(i,j))/ap(i,j)
                  else
                    Tj(i,j)=(aw(i,j)*Tj(i,j-1)+an(i,j)*Tj(i-1,j)+bs(i,j)+api(i,j)*Ti(i,j))/ap(i,j)  
                 endif
                endif
              endif
           enddo
       enddo
     enddo
     
     do i=1,M-1,1!as for the solid,no need to know the inlet and outlet temperature
         do j=1,N,1
            !if(i>0.and.i<M)then
            !    if(j==0)then
            !      this%thermal%Temperature(i,j)=Tj(i,j+1)
            !    else
                  this%thermal%Temperature(i,j)=Tj(i,j)
            !    endif
            !elseif(i==0)then
            !    if(j==0)then
            !      this%thermal%Temperature(i,j)=Tj(i+1,j+1)
            !    elseif(j>0.and.j<N)then
            !      this%thermal%Temperature(i,j)=Tj(i+1,j)
            !    elseif(j==N)then!入口
            !      this%thermal%Temperature(i,j)=Tin
            !    endif
           !elseif(i==M)then
           !     if(j==0)then
           !       this%thermal%Temperature(i,j)=Tj(i-1,j+1)
           !     else
           !       this%thermal%Temperature(i,j)=Tj(i-1,j)
           !     endif
            !endif           
         enddo
     enddo
	 this%th_boundary%T%outlet=this%thermal%Temperature(M-1,N)
end subroutine cal_th_temperature

subroutine solve_temperature(this,flag,Ti,rhoi,dt)
 implicit none
 class(sys_assembly),intent(in out)::this
 real,intent(in)::flag
 real,intent(in)::Ti(:,:)
 real,intent(in)::rhoi(:,:)
 real dt
 !local
 call cal_th_convection(this)
 call cal_th_temperature(this,flag,Ti,rhoi,dt)
end subroutine solve_temperature


subroutine update_property(this,drho)
  implicit none
  class(sys_assembly),intent(in out)::this
  real drho
  !local
  integer i,M,N,Ny
  real,allocatable::rhof(:)!用于存放上一迭代步的密度
  Ny=this%mesh%Ny
  M=Ny+1
  N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
  allocate(rhof(0:M))
      do i=0,M,1
        RHOF(i)=this%property%rho(i,N)
      enddo
  
      do i=1,M-1,1
         this%property%rho(i,N)=get_density(this%thermal%Temperature(i,N))
      enddo
      this%property%rho(0,N)=this%property%rho(1,N)
      this%property%rho(M,N)=this%property%rho(M-1,N)
    
      drho=0.0
      do i=0,M,1
        drho=drho+abs((this%property%rho(i,N)-RHOF(i))/RHOF(i))
      enddo
end subroutine update_property
end module IMP_assembly_header
