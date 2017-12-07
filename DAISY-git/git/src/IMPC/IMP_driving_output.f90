module IMP_driving_output
    use IMP_assm_global
    implicit none
    private
    public::Run_output
contains
    subroutine Run_output()
      !open(1,file='E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\mesh.txt')
      open(1,file='..\output\mesh.txt')
      write(1,*) assm1%mesh%Nf,assm1%mesh%Ng,assm1%mesh%Ns,assm1%mesh%Ny
      close(1) 
      open(2,file='..\output\temperature.txt')
      !write(2,*) assm1%thermal%temperature
      write(2,*)  temperature
      !write(2,*) assm1%pow%power
      close(2) 
      
      open(6,file='..\output\pressure.txt')
      write(6,*) pressure
      close(6) 
      
      open(7,file='..\output\velocity.txt')
      write(7,*) velocity
      close(7) 
      
      open(3,file='..\output\tout.txt')
      write(3,*) timer1%tout
      close(3) 
      open(4,file='..\output\tpow.txt')
      write(4,*) timer1%pow
      close(4) 
      open(5,file='..\output\tuin.txt')
      write(5,*) timer1%uin
      close(5)

      !print*,assm1%th_boundary%p%outlet
      !liquid PVT¡¢rho distribution
      !solid temperature distribution
      print*,assm1%th_boundary%u%inlet,assm1%Thermal%velocity,assm1%th_boundary%u%outlet
      print*,assm1%th_boundary%p%inlet,assm1%Thermal%pressure,assm1%th_boundary%p%outlet
      print*,assm1%th_boundary%T%inlet,assm1%Thermal%temperature(:,assm1%mesh%Nf+assm1%mesh%Ng+assm1%mesh%Ns+1),assm1%th_boundary%T%outlet
    end subroutine Run_output
end module IMP_driving_output
