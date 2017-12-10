module IMP_driving_cal_transient
    use IMP_mathkerel
    implicit none  
    private
    public::solve_momentumA
    public::solve_pmodifyA
contains
    subroutine solve_momentumA(N,A,b,u)
     implicit none
     integer,intent(in)::N
     real,intent(in)::A(:,:)
     real,intent(in)::b(:)
     real,intent(in out)::u(:)
     !local
     integer i
     real,allocatable::uc(:)
     allocate(uc(1:N))
     
     call tdma(N,A,b,uc)
     
     do i=1,N,1
        if(uc(i)>0.0)then
        u(i)=uc(i)
        else
        u(i)=0.0
        endif
      enddo
    end subroutine solve_momentumA
    
subroutine solve_pmodifyA(A,b,pmodify)
  implicit none
  real,intent(in)::A(:,:),b(:)
  real,intent(in out)::pmodify(:)
  !local
  integer M,N,i
  real,allocatable::pmm(:)
  M=size(b)
  N=size(pmodify)
  allocate(pmm(M))
  call tdma(M,A,b,pmm)
  do i=1,N,1
     if(i<N)then
        pmodify(i)=pmm(i)
      else
        pmodify(i)=0.0
      endif
   enddo
end subroutine solve_pmodifyA
end module IMP_driving_cal_transient    