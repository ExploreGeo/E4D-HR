module obj

    use input
    use vars
    use mod_con
    
    implicit none
    real :: phi_tot, phi_data, phi_model, chi2, PHI_D, PHI_M, PLAM
    real :: PHI_T = 0, errm, erms, errm0, erms0, chi20, phi_data0, o_chi2
    real :: PHI_T_prev, PHI_D_prev, PHI_M_prev  ! Previous iteration values for reporting
    real, dimension(15) :: phi = 0
    integer :: ncull
    logical :: con_flag = .false.
    integer, dimension(1) :: inrb
    real, dimension(:), allocatable :: imod_vec
    real, dimension(:), allocatable :: chi2_hist,chi2_change
    integer, dimension(:), allocatable :: inum_vec
    logical, dimension(4) :: check_chidec = .false.
    
    ! Best model tracking for IP inversion
    real :: best_chi2 = huge(1.0)
    integer :: best_iter = -1
    real, dimension(:), allocatable :: best_sigma_re, best_sigma_im
    
    contains
     
        !______________________________________________________________________
        subroutine check_convergence
            implicit none
            integer :: i
            real :: lambda
            logical :: exst

            con_flag = .false.

            if (chi2_hist_size >= 2) then
                ! Allocate chi2_hist if not already allocated
                if (.not. allocated(chi2_hist)) then
                    allocate(chi2_hist(chi2_hist_size))
                    chi2_hist = huge(1.0)  ! Initialize with large values
                end if

                ! Allocate chi2_change if not already allocated
                if (.not. allocated(chi2_change)) then
                    allocate(chi2_change(chi2_hist_size - 1))
                    chi2_change = huge(1.0)  ! Initialize with large values
                end if
            end if

            !! Evaluate the objective function
            call eval_obj
            
            !! Check the chi2 criteria
            if ((chi2 <= norm_chi2)) then
                con_flag = .true.
                return
            end if

            if (chi2_hist_size >= 2) then
                ! Calculate chi2_change as percentage differences after eval_obj updates chi2_hist (e.g. -5.2%)
                do i = 1, chi2_hist_size - 1
                    if (chi2_hist(i) /= 0.0) then
                        chi2_change(i) = ((chi2_hist(i+1) - chi2_hist(i)) / chi2_hist(i)) * 100.0
                    else
                        chi2_change(i) = 0.0
                    end if
                end do

                !! Check if average change in chi2 is >= threshold (default 0)
                if (sum(chi2_change) / (chi2_hist_size - 1) >= chi2_conv_thresh) then
                    con_flag = .true.
                    return
                end if
            end if

        end subroutine check_convergence
        !______________________________________________________________________
        !______________________________________________________________________
        subroutine eval_obj
            implicit none
            
            real, dimension(:), allocatable :: mod_vec, sig_up_par
            real, dimension(nm) :: dat_vec
            integer :: nrow, i, rbi, j, k, n, sizeData
            real :: err_mean, err_sdev
            
            o_chi2 = chi2
            
            if (invi) then
                dat_vec = Wdi * (dobsi - dpredi)
            else
                dat_vec = Wd * (dobs - dpred)
            end if
            phi_data0 = dot_product(dat_vec, dat_vec)
            chi20 = phi_data0 / nm
           
            Wd_cull = 1
            ncull = 0
            if (cull_flag == 1) then
                !! Calculate the error mean and standard deviation
                err_mean = sum(dat_vec) / real(nm)
                err_sdev = sqrt(dot_product((dat_vec - err_mean), (dat_vec - err_mean)) / real(nm))
                
                !! Set the data culling vector
                do i = 1, nm
                    if (dat_vec(i) < (err_mean - cull_dev * err_sdev) .or. dat_vec(i) > err_mean + cull_dev * err_sdev) then
                        Wd_cull(i) = 0
                        ncull = ncull + 1
                    end if
                end do 
            end if
            
            ncull = nm - int(sum(Wd_cull))
            
            if (invi) then
                dat_vec = Wdi * Wd_cull * (dobsi - dpredi)
            else
                dat_vec = Wd * Wd_cull * (dobs - dpred)
            end if
            
            phi_data = dot_product(dat_vec, dat_vec)
            chi2 = phi_data / (nm - ncull)
            
            if (chi2_hist_size >= 2) then
                ! Track best model during DC inversion (not during IP inversion)
                if (.not. invi .and. chi2 < best_chi2) then
                    best_chi2 = chi2
                    best_iter = iter
                    
                    ! Allocate arrays if needed
                    if (.not. allocated(best_sigma_re)) allocate(best_sigma_re(n_elements))
                    if (i_flag .and. .not. allocated(best_sigma_im)) allocate(best_sigma_im(n_elements))
                    
                    ! Save current best model
                    best_sigma_re = sigma_re
                    if (i_flag .and. allocated(sigma_im)) then
                        best_sigma_im = sigma_im
                    end if
                    
                end if
                
                ! Shift values left and add new chi2 at the end
                chi2_hist(1:chi2_hist_size-1) = chi2_hist(2:chi2_hist_size)  
                chi2_hist(chi2_hist_size) = chi2
            end if
         
            if (invi) then
                do i = 1, nm
                    dat_vec(i) = (dobsi(i) - dpredi(i)) * Wdi(i) - 1 ! /dobsi(i)
                end do
            else
                do i = 1, nm
                    dat_vec(i) = (dobs(i) - dpred(i)) * Wd(i) - 1     ! /dobs(i)
                end do
            end if
         
            errm0 = sum(dat_vec / nm)
            erms0 = sqrt(dot_product(dat_vec, dat_vec) / nm)
            errm = sum(dat_vec * Wd_cull / (nm - ncull))
            erms = sqrt(dot_product(dat_vec, dat_vec) / (nm - ncull))
            
            if (mode == 3 .or. res_flag) then
                inrb(1) = maxval(rblock(:, 3))
                if (allocated(imod_vec)) deallocate(imod_vec, inum_vec)
                allocate(imod_vec(inrb(1)), inum_vec(inrb(1)))
                allocate(mod_vec(ccount))
                mod_vec = 0
                imod_vec = 0
                inum_vec = 0
           
                do i = 1, ccount
                    rbi = rblock(i, 3)
                    inum_vec(rbi) = inum_vec(rbi) + 1
                    select case (smetric(rbi, 2))
                    case (1)
                        if (invi) then
                            mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i, 1))) - log(sigma_re(rblock(i, 2))))
                        else
                            mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i, 1))) - log(sigma_re(rblock(i, 2))))
                        end if

                    case (2)
                        if (invi) then
                            !mod_vec(i) = Wm(i) * abs(log(sigmai(rblock(i,1))) - log(sigmai(rblock(i,2))))
                            mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (3)
                        select case (smetric(rbi, 3))
                        case (0)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i, 1))) - C_targ(rbi))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i, 1))) - C_targ(rbi))
                            end if
                            
                        case (1)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i, 1))) - log(refsig(rblock(i,1))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,1))) - log(refsig(rblock(i,1))))
                            end if

                        case (2)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i,1))) - log(prefsig(rblock(i,1))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,1))) - log(prefsig(rblock(i,1))))
                            end if

                        end select

                    case (4)
                        select case (smetric(rbi, 3))
                        case (0)
                            if (invi) then
                                mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - C_targ(rbi))
                            else
                                mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - C_targ(rbi))
                            end if

                        case (1)
                            if (invi) then
                                mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(refsig(rblock(i,1))))
                            else
                                mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(refsig(rblock(i,1))))
                            end if

                        case (2)
                            if (invi) then
                                mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(prefsig(rblock(i,1))))
                            else
                                mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(prefsig(rblock(i,1))))
                            end if

                        end select

                    case (5)
                        if (invi) then
                            mod_vec(i) = Wm(i) * (log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (6)
                        if (invi) then
                            mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (7)
                        select case (smetric(rbi, 3))
                        case (0)
                            if (invi) then
                                mod_vec(i) = Wm(i) * ( (C_targ(rbi) - log(phase(rblock(i,2)))) - &
                                                      (C_targ(rbi) - log(phase(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * ( (C_targ(rbi) - log(sigma_re(rblock(i,2)))) - &
                                                      (C_targ(rbi) - log(sigma_re(rblock(i,1)))))
                            end if

                        case (1)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i,2))) - log(refsig(rblock(i,2))) - &
                                                      (log(phase(rblock(i,1))) - log(refsig(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,2))) - log(refsig(rblock(i,2))) - &
                                                      (log(sigma_re(rblock(i,1))) - log(refsig(rblock(i,1)))))
                            end if

                        case (2)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i,2))) - log(prefsig(rblock(i,2))) - &
                                                      (log(phase(rblock(i,1))) - log(prefsig(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,2))) - log(prefsig(rblock(i,2))) - &
                                                      (log(sigma_re(rblock(i,1))) - log(prefsig(rblock(i,1)))))
                            end if

                        end select

                    case (8)
                        select case (smetric(rbi, 3))
                        case (0)
                            if (invi) then
                                mod_vec(i) = Wm(i) * ( (C_targ(rbi) - log(phase(rblock(i,2)))) - &
                                                      (C_targ(rbi) - log(phase(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * ( (C_targ(rbi) - log(sigma_re(rblock(i,2)))) - &
                                                      (C_targ(rbi) - log(sigma_re(rblock(i,1)))))
                            end if
                            
                        case (1)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i,2))) - log(refsig(rblock(i,2))) - &
                                                      (log(phase(rblock(i,1))) - log(refsig(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,2))) - log(refsig(rblock(i,2))) - &
                                                      (log(sigma_re(rblock(i,1))) - log(refsig(rblock(i,1)))))
                            end if

                        case (2)
                            if (invi) then
                                mod_vec(i) = Wm(i) * (log(phase(rblock(i,2))) - log(prefsig(rblock(i,2))) - &
                                                      (log(phase(rblock(i,1))) - log(prefsig(rblock(i,1)))))
                            else
                                mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,2))) - log(prefsig(rblock(i,2))) - &
                                                      (log(sigma_re(rblock(i,1))) - log(prefsig(rblock(i,1)))))
                            end if

                        end select

                    case (9)
                        if (invi) then
                            mod_vec(i) = Wm(i) * (log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * (log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (10)
                        if (invi) then
                            mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (11)
                        if (invi) then
                            mod_vec(i) = Wm(i) * abs(log(phase(rblock(i,1))) - log(phase(rblock(i,2))))
                        else
                            mod_vec(i) = Wm(i) * abs(log(sigma_re(rblock(i,1))) - log(sigma_re(rblock(i,2))))
                        end if

                    case (12)
                        mod_vec(i) = Wm(i) * log(sigma_re(rblock(i,1))) * cg_wts(rblock(i,2), 1)
                        do j = 1, 4
                            k = neighbors(rblock(i,1), j)
                            if (k > 0) then
                                mod_vec(i) = mod_vec(i) + Wm(i) * log(sigma_re(k)) * cg_wts(rblock(i,2), j + 1)
                            end if
                        end do

                    case DEFAULT
                    end select
                    imod_vec(rbi) = imod_vec(rbi) + abs(mod_vec(i))

                end do
            end if
           
            phi_model = beta * dot_product(mod_vec, mod_vec)    
            phi_tot = phi_data + phi_model
            deallocate(mod_vec)
              
            if (iter == 0) then
                PHI_T = phi_tot
                PHI_D = phi_data
                PHI_M = phi_model
                ! Initialize previous values for first iteration
                PHI_T_prev = phi_tot
                PHI_D_prev = phi_data
                PHI_M_prev = phi_model
            end if

            !! Check for hemstitching and divergence
            if (iter > 0 .and. iter < 5) then
                hemstiching = .false.
                if (chi2 > o_chi2) then
                    check_chidec(iter) = .false.
                else
                    check_chidec(iter) = .true.
                end if
            elseif (iter > 4) then
                do i = 1, 3
                    check_chidec(i) = check_chidec(i + 1)
                end do
                check_chidec(4) = .true.
                if (chi2 > o_chi2) then
                    check_chidec(4) = .false.
                end if

                if ((check_chidec(2) .neqv. check_chidec(1)) .and. &
                    (check_chidec(3) .neqv. check_chidec(2)) .and. &
                    (check_chidec(4) .neqv. check_chidec(3))) then
                    hemstiching = .true.
                end if
            end if

            if (iter < 3) then
                diverging = .false.
            else
                if (.not. check_chidec(1) .and. &
                    .not. check_chidec(2) .and. &
                    .not. check_chidec(3)) then
                    diverging = .true.
                end if
            end if
            
        end subroutine eval_obj 
        !______________________________________________________________________
        
        !______________________________________________________________________
        subroutine check_beta
            implicit none
            real :: lambda

            !! Store previous values for reporting before updating
            PHI_T_prev = PHI_T
            PHI_D_prev = PHI_D
            PHI_M_prev = PHI_M

            !! Check to see if we need to change beta yet
            if (abs((PHI_T - phi_tot) / PHI_T) <= del_obj) then
                lambda = 1 - (phi_data - norm_chi2 * nm) / (norm_chi2 * nm)
                !if(beta_red > lambda) lambda = beta_red
                lambda = beta_red
                !! If we're here then we've found the solution at this beta value
                !! If conv_opt = 2 then we're done
                if (conv_opt == 2) then
                    con_flag = .true.
                    return
                end if
                beta = lambda * beta
                phi_model = lambda * phi_model
            end if
            
            !! Update the objective function
            PHI_T = phi_tot
            PHI_D = phi_data
            PHI_M = phi_model

        end subroutine check_beta
        !______________________________________________________________________
        
        !______________________________________________________________________
        subroutine restore_best_dc_model()
            implicit none
            
            if (allocated(best_sigma_re) .and. best_iter >= 0) then
                write(*,*)
                write(*,*) "  Restoring best DC model from DC inversion"
                write(*,*) "  best iteration:",best_iter,"  best chi2:",best_chi2
                write(*,*) "  current chi2:",chi2
                
                ! Restore the best DC model
                sigma_re = best_sigma_re
                
                ! Update the phase based on the restored conductivity if we have complex data
                if (i_flag .and. allocated(best_sigma_im) .and. allocated(phase)) then
                    ! Calculate phase from the best imaginary conductivity
                    phase = atan(best_sigma_im / best_sigma_re)
                    sigma_im = best_sigma_im
                end if
                
            else
                write(*,*) "Warning: No best DC model found, using final DC model for IP inversion"
            end if
            
        end subroutine restore_best_dc_model
        !______________________________________________________________________

end module obj
 