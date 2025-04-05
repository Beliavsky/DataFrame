module m1_mod
implicit none
interface foo
   module procedure foo_mat
end interface foo
contains

subroutine foo_mat(x, nacf, label)
real, intent(in) :: x(:,:)
integer, intent(in) :: nacf
character (len=*), intent(in), optional :: label
end subroutine foo_mat

end module m1_mod

module m2_mod
use m1_mod, only: foo
implicit none
interface foo
  module procedure foo_df
end interface foo
contains

impure elemental subroutine foo_df(nacf, outu, xstr)
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: xstr
if (present(xstr)) print*,"in foo_df, xstr = '" // trim(xstr) // "'"
end subroutine foo_df

impure elemental subroutine bar(nacf, outu, xstr)
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: xstr
if (present(xstr)) print*,"in bar, xstr = '" // trim(xstr) // "'"
end subroutine bar

end module m2_mod

program main
use m2_mod, only: foo, bar
implicit none
call bar(nacf = 5, xstr="ab")
call foo(nacf = 5, xstr="ab")
end program main
