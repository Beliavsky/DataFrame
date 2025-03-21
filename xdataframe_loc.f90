program xdataframe_loc
use dataframe_mod, only: DataFrame, display, random
type(DataFrame) :: df
call random(df, 4, 3)
df%index = df%index * 10
call display(df, title="df")
! subset by row and column numbers
call display(df%icol([2,3]), title="df%icol([1,3])")
call display(df%irow([2,4]), title="df%irow([2,4])")
! subset by index values and column names
call display(df%loc(rows=[20, 40]), title="df%loc(rows=[20, 40]")
call display(df%loc(columns=["C1", "C3"]), title="df%loc(columns=['C1', 'C3'])")
call display(df%loc(rows=[20, 40], columns=["C1", "C3"]), &
             title="df%loc(rows=[20, 40], columns=['C1', 'C3']")
call display(df%loc())
end program xdataframe_loc
