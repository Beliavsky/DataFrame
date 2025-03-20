executables = xbasic_stats_gfort.exe xdataframe_loc_gfort.exe xxdataframe_stats_gfort.exe xprint_table_gfort.exe xtable_stats_gfort.exe xxtable_gfort.exe xutil_gfort.exe xxdataframe_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o dataframe.o xdataframe_loc.o table.o table_stats.o dataframe_stats.o xxdataframe_stats.o xprint_table.o xtable_stats.o xxtable.o xutil.o xxdataframe.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xbasic_stats_gfort.exe: kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o
	$(FC) -o xbasic_stats_gfort.exe kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o $(FFLAGS)

xdataframe_loc_gfort.exe: kind.o util.o dataframe.o xdataframe_loc.o
	$(FC) -o xdataframe_loc_gfort.exe kind.o util.o dataframe.o xdataframe_loc.o $(FFLAGS)

xxdataframe_stats_gfort.exe: kind.o util.o basic_stats.o dataframe.o table.o table_stats.o dataframe_stats.o xxdataframe_stats.o
	$(FC) -o xxdataframe_stats_gfort.exe kind.o util.o basic_stats.o dataframe.o table.o table_stats.o dataframe_stats.o xxdataframe_stats.o $(FFLAGS)

xprint_table_gfort.exe: kind.o util.o xprint_table.o
	$(FC) -o xprint_table_gfort.exe kind.o util.o xprint_table.o $(FFLAGS)

xtable_stats_gfort.exe: kind.o util.o basic_stats.o table.o table_stats.o xtable_stats.o
	$(FC) -o xtable_stats_gfort.exe kind.o util.o basic_stats.o table.o table_stats.o xtable_stats.o $(FFLAGS)

xxtable_gfort.exe: kind.o util.o table.o xxtable.o
	$(FC) -o xxtable_gfort.exe kind.o util.o table.o xxtable.o $(FFLAGS)

xutil_gfort.exe: kind.o util.o xutil.o
	$(FC) -o xutil_gfort.exe kind.o util.o xutil.o $(FFLAGS)

xxdataframe_gfort.exe: kind.o util.o dataframe.o xxdataframe.o
	$(FC) -o xxdataframe_gfort.exe kind.o util.o dataframe.o xxdataframe.o $(FFLAGS)

run: $(executables)
	./xbasic_stats_gfort.exe
	./xdataframe_loc_gfort.exe
	./xxdataframe_stats_gfort.exe
	./xprint_table_gfort.exe
	./xtable_stats_gfort.exe
	./xxtable_gfort.exe
	./xutil_gfort.exe
	./xxdataframe_gfort.exe

clean:
	rm -f $(executables) $(obj)

