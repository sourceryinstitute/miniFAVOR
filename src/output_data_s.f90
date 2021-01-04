submodule(output_data_m) output_data_s
  use assertions_interface, only : assert

  implicit none

contains

  module procedure default_constructor
     new_output_data_t%input_data_ = input_data_t()
     new_output_data_t%R_Tndt_ = [0.]
     new_output_data_t%K_hist_ = [0.]
     new_output_data_t%Chemistry_content_ = reshape([0.], [1,1])
     new_output_data_t%Chemistry_factor_ = [0.]
     new_output_data_t%CPI_ = [0.]
     new_output_data_t%CPI_avg_ = [0.]
  end procedure

  module procedure assign

    call assert(same_type_as(self, rhs), "output_data_t%assign: same_type_as(self, rhs)")

    select type(rhs)
      type is(output_data_t)
        self%input_data_          = rhs%input_data_
        self%R_Tndt_              = rhs%R_Tndt_
        self%K_hist_              = rhs% K_hist_
        self%Chemistry_content_   = rhs%Chemistry_content_
        self%Chemistry_factor_    = rhs%Chemistry_factor_
        self%CPI_                 = rhs%CPI_
        self%CPI_avg_             = rhs%CPI_avg_
      class default
        error stop "output_data_t%assign: unsupported rhs type"
    end select

  end procedure

  module procedure norm
    !! compute L-infinity norm
    norm_of_self = maxval(abs([ &
      self%input_data_%norm(), self%R_Tndt_, self%K_hist_ , self%Chemistry_content_, self%Chemistry_factor_, self%CPI_, &
      self%CPI_avg_ &
    ]))
  end procedure

  module procedure subtract
    type(output_data_t) local_difference

    call assert(same_type_as(self, rhs), "output_data_t%subtract: same_type_as(self, rhs)")

    select type(rhs)
      type is(output_data_t)
        local_difference%input_data_        = self%input_data_          - rhs%input_data_
        local_difference%R_Tndt_            = self%R_Tndt_              - rhs%R_Tndt_
        local_difference%K_hist_            = self%K_hist_              - rhs% K_hist_
        local_difference%Chemistry_content_ = self%Chemistry_content_   - rhs%Chemistry_content_
        local_difference%Chemistry_factor_  = self%Chemistry_factor_    - rhs%Chemistry_factor_
        local_difference%CPI_               = self%CPI_                 - rhs%CPI_
        local_difference%CPI_avg_           = self%CPI_avg_             - rhs%CPI_avg_
      class default
        error stop "output_data_t%subtract: rhs type unsupported"
    end select

    difference = local_difference
  end procedure

  module procedure new_output_data
     new_output_data%input_data_ = input_data
     new_output_data%R_Tndt_ = R_Tndt
     new_output_data%K_hist_ = K_hist
     new_output_data%Chemistry_content_ = Chemistry_content
     new_output_data%Chemistry_factor_ = Chemistry_factor
     new_output_data%CPI_ = CPI
     new_output_data%CPI_avg_ = CPI_avg
  end procedure

  module procedure write_formatted

      associate(nsim => self%input_data_%nsim())

        !Write important outputs summary
        write (unit, '(2a)') 'MiniFAVOR Output Summary', new_line('a')
        write (unit, '(2a)') '/Key inputs/', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Crack Depth: ', self%input_data_%a(), ' in', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Vessel Thickness: ', self%input_data_%b(), ' in', new_line('a')
        write (unit, '(a25,i10,a)') 'Number of Simulations: ', nsim, new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Copper Content: ', self%input_data_%Cu_ave(), ' %', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Nickel Content: ', self%input_data_%Ni_ave(), ' %', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Copper Content STDEV: ', self%input_data_%Cu_sig(), ' %', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Nickel Content STDEV: ', self%input_data_%Ni_sig(), ' %', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'ID Surface Fluence: ', self%input_data_%fsurf(), ' n/cm^2', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Unirradiated RTndt: ', self%input_data_%RTndt0(), ' degF', new_line('a')
        write (unit, '(2a)') '/Results/', new_line('a')

        write (unit, '(a25,f10.3,a)') 'Final CPI: ', self%CPI_avg_(nsim), new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Minimum crack tip RTndt: ', &
            minval(self%R_Tndt_), ' degF', new_line('a')
        write (unit, '(a25,f10.3,2a)') 'Maximum crack tip  RTndt: ', &
            maxval(self%R_Tndt_), ' degF', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Average crack tip RTndt: ', &
            sum(self%R_Tndt_)/nsim, ' degF'
      end associate

  end procedure write_formatted

  module procedure R_Tndt
   my_R_Tndt = self%R_Tndt_
  end procedure

  module procedure K_hist
   my_K_hist = self%K_hist_
  end procedure

  module procedure Chemistry_content
    my_Chemistry_content = self%Chemistry_content_
  end procedure

  module procedure Chemistry_factor
    my_Chemistry_factor = self%Chemistry_factor_
  end procedure

  module procedure CPI
    my_CPI = self%CPI_
  end procedure

  module procedure CPI_avg
    my_CPI_avg = self%CPI_avg_
  end procedure

  module procedure nsim
    my_nsim = self%input_data_%nsim()
  end procedure

  module procedure ntime
    my_ntime = self%input_data_%ntime()
  end procedure

end submodule output_data_s
