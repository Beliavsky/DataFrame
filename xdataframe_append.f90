program xdataframe_append
use dataframe_mod, only: DataFrame, random, display
implicit none
type(DataFrame) :: df
call random(df, 3, 2)
call display(df)
call df%append_col("10*C2", 10*df%values(:,2))
call display(df)
call df%set_col("C1", 100*df%values(:,1))
call display(df)
call df%set_col("C3", 10*df%values(:,1))
call display(df)
call df%set_col("C3", [10.0d0, 20.0d0])  ! will cause run-time error
call df%append_col("abc", [10.0d0, 20.0d0]) ! will cause run-time error
call display(df)
end program xdataframe_append