program xacf
use kind_mod, only: dp
use random_mod, only: random_normal
use basic_stats_mod, only: acf, print_acf
implicit none
integer, parameter :: n = 10**6, ncol = 2, nacf = 3
real(kind=dp) :: x(n, ncol), ar1(ncol), xxacf(nacf, ncol)
integer :: icol, j
call random_seed()
x = random_normal(n, ncol)
ar1 = [-0.7, 0.5]
print "(*(a8))", "", "AR1", "ACF_1", "ACF_2", "ACF_3"
do icol=1,ncol
   do j=2,n
      x(j,icol) = x(j,icol) + ar1(icol)*x(j-1,icol)
   end do
   print "(i8,*(f8.4))", icol, ar1(icol), acf(x(:,icol), nacf)
end do
xxacf = acf(x, nacf)
do icol=1,ncol
   print "(i8,*(f8.4))", icol, ar1(icol), xxacf(:, icol)
end do
! print autocorrelations of columns of array
call print_acf(x, nacf, labels = ["a", "b"], title="autocorr", &
   fmt_header="()", fmt_trailer = "('done')")
! print autocorrelations of univariate data
call print_acf(x(:,1), nacf, label = "a", title="autocorr", &
   fmt_header="()", fmt_trailer = "('done')")
end program xacf
