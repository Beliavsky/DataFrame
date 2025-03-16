program xbasic_stats
use        kind_mod, only: dp
use basic_stats_mod, only: mean, sd, variance, mean_and_sd, correl, &
                           stats, corr_mat, rms, moving_sum, &
                           moving_average, print_corr_mat
use      random_mod, only: random_normal
use        util_mod, only: display
integer, parameter :: n = 10**6, ncol=3
real(kind=dp) :: x(n), x1(n), x2(n), xmat(n, ncol), xran(3, 4), &
   xcorr(ncol, ncol)
real(kind=dp), allocatable :: y(:), ysum(:), yma(:)
integer :: k
logical, parameter :: test_moving_sum_average = .true.
x = 3.0_dp + 10.0_dp*random_normal(n)
print "(*(a12))", "mean", "sd", "variance", "mean", "sd", "rms"
print "(*(f12.6))", mean(x), sd(x), variance(x), mean_and_sd(x), rms(x)
print "(*(f12.6))", stats([character(len=8) :: "mean", "sd", "variance"], x)
x1 = random_normal(n)
x2 = x1 + random_normal(n)
print*,"correlation:", correl(x1, x2)
call random_number(xran)
call display(xran, fmt_header="(/,'random matrix')")
call display(xran, fmt_r="(*(1x,f6.2))")
xmat = random_normal(n, 3)
xmat(:,3) = xmat(:,3) + xmat(:,1) + 2*xmat(:,2)
call display(corr_mat(xmat), fmt_header="(/,'correlation matrix')")
do icol=1,ncol
   do jcol=1,ncol
      xcorr(icol,jcol) = correl(xmat(:,icol), xmat(:,jcol))
   end do
end do
call display(xcorr, fmt_header="(/,'check correlation matrix')")
if (test_moving_sum_average) then
   y = [10, 20, 30, 40]
   print "(/,*(f8.2))", y
   do k=0,5
      ysum = moving_sum(y, k)
      yma  = moving_average(y, k)
      print "('sum', i2, *(f8.2))", k, ysum
      print "('avg', i2, *(f8.2))", k, yma
   end do
end if
call print_corr_mat(xmat, col_names=["a", "b", "c"], &
   fmt_header="(/,'correlations')", fmt_trailer="('end', /)")
call print_corr_mat(xmat, col_names=["a", "b", "c"], &
   fmt_header="(/,'correlations')", fmt_col_names="(*(a6,:,1x))", &
   fmt_row="(a6, *(1x,f6.2))")
end program xbasic_stats
