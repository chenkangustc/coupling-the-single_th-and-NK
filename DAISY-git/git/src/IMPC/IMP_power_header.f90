module IMP_power_header
    contains
    subroutine update_power(power,fq_core,ltime,ctime)
       implicit none
       real,intent(in out)::power(:,:)
       real,intent(in out)::fq_core(:,:)
       real,intent(in)::ltime
       real,intent(in)::ctime
       !local
       if (ctime>0.and.ctime<=2.0) then
           power=power-0.1*(ctime-ltime)*power
       elseif(ctime>2.0.and.ctime<=4.0) then
           power=power-0.35*(ctime-ltime)*power
       elseif(ctime>4.0.and.ctime<=14) then
           power=power-0.01*(ctime-ltime)*power
       elseif(ctime>14.0)then
           power=0.0
       endif
       print*,'power=',power(1,1)
    end subroutine update_power
end module IMP_power_header