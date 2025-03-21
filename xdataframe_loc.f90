program xdataframe_loc
use dataframe_mod, only: DataFrame, display, random
type(DataFrame) :: df
call random(df, 5, 4)
df%index = df%index * 10
call display(df, title="df", fmt_trailer="()")
call display(df%icol([2,3]), title="df%icol([2,3]", fmt_trailer="()")
call display(df%irow([2,4]), title="df%irow([2,4])")
call display(df%loc(rows=[20, 40]))
call display(df%loc(columns=["C2", "C3"]))
call display(df%loc(rows=[20, 40], columns=["C2", "C3"]))
call display(df%loc())
end program xdataframe_loc
