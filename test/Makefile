

FC = gfortran
# FC = mpiifort
INC = -I/usr/include
FFLAGS = -L/usr/lib/x86_64-linux-gnu -lnetcdff


run : test.x
	./$<

test.x : ../EasyNC.f90 test.f90 
	$(FC) -o $@ $^ $(INC) $(FFLAGS)
	
testL.x : ../EasyNC.f90 test-logical.f90
	$(FC) -o $@ -g $^ $(INC) $(FFLAGS)
runL : testL.x
	./$<


runT : testT.x
	./$<
	
testT.x : ../EasyNC.f90 test-type.f90
	$(FC) -o $@ -g $^ $(INC) $(FFLAGS)


testC.x : ../EasyNC.f90 test-char.f90
	$(FC) -o $@ -g $^ $(INC) $(FFLAGS)
runC : testC.x
	./testC.x
checkC : 
	ncdump testC.nc


testA.x : ../EasyNC.f90 test-allocate.f90
	$(FC) -o $@ -g $^ $(INC) $(FFLAGS)
runA : testA.x
	./testA.x
checkA : 
	ncdump testA.nc
	


clean : 
	rm -f *.x *.nc *.mod *.o
