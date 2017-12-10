module IMP_property
    contains
    function get_density(tin) result(density)
     real,intent(in)::tin
     real::density
     density=11096-1.3326*tin
    end function get_density
end module IMP_property
