exec   = dst_gfort.exe
obj    = kind.o util.o basic_stats.o table.o table_stats.o dataframe.o dataframe_stats.o xdataframe_stats.o
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private

all: $(exec)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

$(exec): $(obj)
	$(FC) -o $(exec) $(obj) $(FFLAGS)

run: $(exec)
	./$(exec)

clean:
	rm -f $(exec) $(obj)

