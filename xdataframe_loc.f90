program xdataframe_loc
use dataframe_mod, only: DataFrame, display, random
type(DataFrame) :: df
call random(df, 5, 4)
call display(df, title="df", fmt_trailer="()")
call display(df%icol([2,3]), title="df%icol([2,3]", fmt_trailer="()")
call display(df%irow([2,4]), title="df%irow([2,4])")
end program xdataframe_loc
