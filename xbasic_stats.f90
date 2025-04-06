program xbasic_stats
use        kind_mod, only: dp
use basic_stats_mod, only: mean, sd, variance, mean_and_sd, correl, &
                           stats, corr_mat, rms, moving_sum, &
                           moving_average, print_corr_mat, print_cov_mat
use      random_mod, only: random_normal
use        util_mod, only: display
implicit none
integer, parameter :: n = 10**6, ncol=3
real(kind=dp) :: x(n), x1(n), x2(n), xmat(n, ncol), xran(3, 4), &
   xcorr(ncol, ncol)
real(kind=dp), allocatable :: y(:), ysum(:), yma(:)
integer :: icol, jcol, k
logical, parameter :: test_moving_sum_average = .true.
real(kind=dp), parameter :: c0 = 3.0_dp, c1 = 10.0_dp
x = c0 + c1*random_normal(n)
print "('c1, c2 = ',2f10.4)", c0, c1
print "(/,*(a12))", "mean", "sd", "variance", "mean", "sd", "rms"
print "(*(f12.6))", mean(x), sd(x), variance(x), mean_and_sd(x), rms(x)
print "(*(f12.6))", stats([character(len=8) :: "mean", "sd", "variance"], x)
x1 = random_normal(n)
x2 = x1 + random_normal(n)
print "(/,a, f8.4)", "correlation of z1, z1+z2: ", correl(x1, x2)
call random_number(xran)
call display(xran, fmt_header="(/,'random matrix')", fmt_trailer="()")
call display(xran, fmt_r="(*(1x,f6.2))", title='fmt_r="(*(1x,f6.2))"')
xmat = random_normal(n, 3)
xmat(:,3) = xmat(:,3) + xmat(:,1) + 2*xmat(:,2)
call display(corr_mat(xmat), fmt_header="(/,'correlation matrix')")
do icol=1,ncol
   do jcol=1,ncol
      xcorr(icol,jcol) = correl(xmat(:,icol), xmat(:,jcol))
   end do
end do
call display(xcorr, fmt_header="(/,'check correlation matrix')")
call print_corr_mat(xmat, col_names=["a", "b", "c"], &
   fmt_header="(/,'correlations')", fmt_trailer="('end', /)")
call print_corr_mat(xmat, col_names=["a", "b", "c"], &
   fmt_header="(/,'correlations')", fmt_col_names="(*(a6,:,1x))", &
   fmt_row="(a6, *(1x,f6.2))")
call print_cov_mat(xmat, col_names=["a", "b", "c"], &
   fmt_header="(/,'covariances')", fmt_col_names="(*(a6,:,1x))", &
   fmt_row="(a6, *(1x,f6.2))")

if (test_moving_sum_average) then
   y = [10, 20, 30, 40]
   print "(/,'y = ',*(f8.2))", y
   do k=0,5
      ysum = moving_sum(y, k)
      yma  = moving_average(y, k)
      print "('sum', i2, *(f8.2))", k, ysum
      print "('avg', i2, *(f8.2))", k, yma
   end do
end if
end program xbasic_stats
