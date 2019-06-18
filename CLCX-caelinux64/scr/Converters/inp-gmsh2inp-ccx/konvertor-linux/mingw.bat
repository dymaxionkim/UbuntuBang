SET PATH=C:\MinGW\bin;C:\MinGW\lib\gcc\mingw32\4.8.1;%PATH%
SET LIBRARY_PATH=C:\MinGW\lib;C:\MinGW\lib\gcc\mingw32\4.8.1

konvertor: konvertor.cpp
	g++ konvertor.cpp -static -o konvertor

clean:
	rm konvertor


pause