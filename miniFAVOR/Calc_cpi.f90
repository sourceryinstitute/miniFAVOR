module calc_cpi
    
implicit none
    
    contains
        
    !Function to calculate cpi(t)
    function cpi_t(K, RTndt, T)
            
        !Variables
        real :: cpi_t
        real, intent(in) :: K, RTndt, T
        real :: aKic, bKic, cKic
            
        !Calculate aKic, bKic, cKic
        aKic = 19.35+8.335*exp(0.02254*(T-RTndt))
        bKic = 15.61+50.132*exp(0.008*(T-RTndt))
        cKic = 4.0
            
        !Calculate cpi_t
        if (K < aKic) then
            cpi_t = 0.0
        else
            cpi_t = 1-exp(-((K-aKic)/bKic)**cKic)
        end if
            
    end function cpi_t
        
end module calc_cpi