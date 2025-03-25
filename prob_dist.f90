module prob_dist_mod
  ! log probability densities and parameter estimation for the normal and lognormal distributions
  use kind_mod, only: dp
  use constants_mod, only: pi
  implicit none
  private
  public :: fit_normal_lognormal
contains

  pure subroutine fit_normal(x, mu, sigma)
    !! fit normal distribution to data
    real(dp), intent(in)  :: x(:)
    real(dp), intent(out) :: mu, sigma
    integer :: n
    real(dp) :: sum_x, sumsq

    n = size(x)
    sum_x = sum(x)
    mu = sum_x / real(n, dp)
    sumsq = sum((x - mu)**2)
    sigma = sqrt(sumsq / real(n, dp))
  end subroutine fit_normal

  pure function ll_normal(x, mu, sigma) result(ll)
    !! log-likelihood of normal distribution with specified mean and standard deviation
    real(dp), intent(in)  :: x(:)
    real(dp), intent(in)  :: mu, sigma
    real(dp)              :: ll
    integer :: n
    n = size(x)
    ll = -0.5_dp * real(n, dp) * log(2.0_dp * pi * sigma**2) &
         - sum((x - mu)**2) / (2.0_dp * sigma**2)
  end function ll_normal

  pure subroutine fit_lognormal(x, mu_log, sigma_log)
    !! fit lognormal distribution to data
    real(dp), intent(in)  :: x(:)
    real(dp), intent(out) :: mu_log, sigma_log
    integer :: n
    real(dp), allocatable :: lx(:)
    n = size(x)
    lx = log(x)
    mu_log = sum(lx) / real(n, dp)
    sigma_log = sqrt(sum((lx - mu_log)**2) / real(n, dp))
  end subroutine fit_lognormal

  pure function ll_lognormal(x, mu_log, sigma_log) result(ll)
    !! log-likelihood of lognormal distribution
    real(dp), intent(in)  :: x(:) !! data
    real(dp), intent(in)  :: mu_log !! mean of log(data)
    real(dp), intent(in)  :: sigma_log !! sd of log(data)
    real(dp)              :: ll
    integer :: n
    real(dp), allocatable :: lx(:)
    n = size(x)
    lx = log(x)
    ll = - sum(log(x)) &
         - 0.5_dp * real(n, dp) * log(2.0_dp * pi * sigma_log**2) &
         - sum((lx - mu_log)**2) / (2.0_dp * sigma_log**2)
  end function ll_lognormal

  subroutine fit_normal_lognormal(x, ll_norm, ll_log)
    !! fit normal and lognormal distributions to data
    real(dp), intent(in) :: x(:)
    real(dp), intent(out) :: ll_norm, ll_log
    real(dp) :: mu_normal, sigma_normal
    real(dp) :: mu_lognormal, sigma_lognormal
    logical  :: print_fits_
    character (len=*), parameter :: fmt_cr = "(a30, 1x, f10.4)"
    print_fits_ = .true.
    call fit_normal(x, mu_normal, sigma_normal)
    ll_norm = ll_normal(x, mu_normal, sigma_normal)

    call fit_lognormal(x, mu_lognormal, sigma_lognormal)
    ll_log = ll_lognormal(x, mu_lognormal, sigma_lognormal)
    
    if (print_fits_) then
       print *, "Normal Distribution Fit:"
       print fmt_cr, "  Mean (mu) =", mu_normal
       print fmt_cr, "  Standard Deviation (sigma) =", sigma_normal
       print fmt_cr, "  Log Likelihood =", ll_norm
       print fmt_cr, "Lognormal Distribution Fit:"
       print fmt_cr, "  Log Mean (mu_log) =", mu_lognormal
       print fmt_cr, "  Log Std Dev (sigma_log) =", sigma_lognormal
       print fmt_cr, "  Log Likelihood =", ll_log
       if (ll_log > ll_norm) then
          print *, "Best fitting distribution: Lognormal"
       else
          print *, "Best fitting distribution: Normal"
       end if
    end if
  end subroutine fit_normal_lognormal

end module prob_dist_mod