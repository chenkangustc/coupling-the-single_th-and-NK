module IMP_re_input_header
    implicit none
    type,public::sys_re_input
       !public
       integer nf,ng,ns,ny,npin
       real xf,xg,xs,xos,acf,height,f,pd,pout
       real Tin,uin,pin
       real Ti,ui,pi
       real alpha,sigma
    contains
     procedure,public::set=>set_inputdata
     procedure,public::publish=>print_inputdata
    end type sys_re_input
     private::set_inputdata
     private::print_inputdata
    contains
     subroutine set_inputdata(this)
      implicit none
      class(sys_re_input)::this
       open(unit=1,file='..\input\re_input.txt')
       !read(1,*) xf,xg,xs,height,nf,ng,ns,ny,f,Tin,pout,Tic,uic,tmax,nt,sigma,sigmab,alpha
       read(1,*) this%xf,this%xg,this%xs,this%xos,this%acf,this%height,this%pd,this%npin,this%nf,this%ng,this%ns,this%ny,this%f,this%Tin,this%pout,this%uin,this%pin,this%Ti,this%ui,this%pi,this%alpha,this%sigma
       close(1)
       write(*,*)'read the data from the input file:'
       write(*,*) this%xf,this%xg,this%xs,this%xos,this%acf,this%height,this%pd,this%npin,this%nf,this%ng,this%ns,this%ny,this%f,this%Tin,this%pout,this%uin,this%pin,this%Ti,this%ui,this%pi,this%alpha,this%sigma
     end subroutine set_inputdata
     
     subroutine print_inputdata(this)
         implicit none
         class(sys_re_input)::this
         print*, "rFuel=",this%xf,"GasGap=",this%xg,"ShellThick=",this%xs,"AssmShellThick=",this%xos,"AcrossFlat=",this%acf,"Height=",this%height,"pd=",this%pd
         print*,"n_pin=",this%npin,"Nf=",this%nf,"Ng=",this%ng,"Ns=",this%ns,"Ny=",this%ny
         print*,"Fric=",this%f
         print*,"Tin=",this%Tin,"Pout=",this%pout
         print*,"Ti=",this%Ti,"ui=",this%ui,"pi=",this%pi,"uin=",this%uin,"pin=",this%pin
         print*,"alpha=",this%alpha,"sigma=",this%sigma
     end subroutine print_inputdata
end module IMP_re_input_header