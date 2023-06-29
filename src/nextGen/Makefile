
FC = gfortran

NC4FORTRAN = /home/roadelse/softwares/mana/nc4fortran/src
RDEE_FORTRAN = '/mnt/c/Users/Roadelse/OneDrive/Roadelse/Tech/proj/~Fortran/rdee_froran'

INC = -I$(NC4FORTRAN) -I$(RDEE_FORTRAN)

all : libeasync.a

libeasync.a : EasyNC.o
	ar rcs $@ $^

	
%.o : %.F90
	$(FC) -c $< $(INC)
