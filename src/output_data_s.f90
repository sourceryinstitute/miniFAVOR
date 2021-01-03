submodule(output_data_m) output_data_s

  implicit none

contains

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
        write (unit, '(a)') 'MiniFAVOR Output Summary', new_line('a')
        write (unit, '(a)') '/Key inputs/', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Crack Depth: ', self%input_data_%a(), ' in', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Vessel Thickness: ', self%input_data_%b(), ' in', new_line('a')
        write (unit, '(a25,i10)') 'Number of Simulations: ', nsim, new_line('a')
        write (unit, '(a25,f10.3,a)') 'Copper Content: ', self%input_data_%Cu_ave(), ' %', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Nickel Content: ', self%input_data_%Ni_ave(), ' %', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Copper Content STDEV: ', self%input_data_%Cu_sig(), ' %', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Nickel Content STDEV: ', self%input_data_%Ni_sig(), ' %', new_line('a')
        write (unit, '(a25,f10.3,a)') 'ID Surface Fluence: ', self%input_data_%fsurf(), ' n/cm^2', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Unirradiated RTndt: ', self%input_data_%RTndt0(), ' degF', new_line('a')
        write (unit, '(a)') '/Results/', new_line('a')

        write (unit, '(a25,f10.3)') 'Final CPI: ', self%CPI_avg_(nsim), new_line('a')
        write (unit, '(a25,f10.3,a)') 'Minimum crack tip RTndt: ', &
            minval(self%R_Tndt_), ' degF', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Maximum crack tip  RTndt: ', &
            maxval(self%R_Tndt_), ' degF', new_line('a')
        write (unit, '(a25,f10.3,a)') 'Average crack tip RTndt: ', &
            sum(self%R_Tndt_)/nsim, ' degF', new_line('a')
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
