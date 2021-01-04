submodule(detailed_output_m) detailed_output_s

  implicit none

contains

  module procedure new_detailed_output
    new_detailed_output%output_data_t = output_data
  end procedure

  module procedure write_formatted

      integer i

      associate(nsim => self%nsim())

        write (unit, '(2a)') 'MiniFAVOR Detailed Output', new_line('a')
        write (unit, '(2a)') '/Applied SIF (ksi*in^0.5)/', new_line('a')

        associate(K_hist => self%K_hist(), ntime => self%ntime())
          write_SIF: &
          do i = 1, ntime
            write (unit, '(f10.3,2a)') K_hist(i), new_line('a')
          end do write_SIF
        end associate

        write (unit, '(2a)') '/Chemistry Results', new_line('a')
        write (unit, '(2a)') 'Cu content (%),  Ni Content (%), Chemistry Factor CF', new_line('a')

        associate(Chemistry_Content => self%Chemistry_content(), Chemistry_factor => self%Chemistry_factor())
          write_chem: &
          do i = 1, nsim
            write (unit, '(3f10.3,2a)') Chemistry_content(i,1), Chemistry_content(i,2), Chemistry_factor(i), new_line('a')
          end do write_chem
        end associate

        write (unit, '(2a)') '/Vessel CPI data', new_line('a')
        write (unit, '(2a)') 'Vessel RTndt (degF),  Vessel CPI, Cumulative Average CPI', new_line('a')

        associate(R_Tndt => self%R_Tndt(), CPI => self%CPI(), CPI_Avg => self%CPI_avg())
          write_CPI: &
          do i = 1, nsim
            write (unit, '(3f10.3,2a)') R_Tndt(i), CPI(i), CPI_avg(i), new_line('a')
          end do write_CPI
        end associate

      end associate

  end procedure write_formatted

end submodule detailed_output_s
