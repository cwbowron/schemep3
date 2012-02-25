MZC=/cygdrive/c/Program\ Files/PLT/mzc.exe
TAGLIB_INCLUDE=/dev/api/taglib-1.5-msvc-bin/include/taglib

default: taglib_more.dll
	cp taglib_more.dll ../../compiled/native/win32/i386/3m

clean:
	rm -f taglib_more.dll
	rm -f taglib_more.obj
	rm -f ../../compiled/native/win32/i386/3m/taglib_more.dll
	
taglib_more.dll: taglib_more.obj 
	$(MZC) --ld taglib_more.dll taglib_more.obj user32.lib tag.lib
	
taglib_more.obj:	taglib_more.cpp
	$(MZC) --cc ++ccf -I$(TAGLIB_INCLUDE) taglib_more.cpp
	
cgc:
	$(MZC) --cgc --cc ++ccf -I$(TAGLIB_INCLUDE) taglib_more.cpp
	$(MZC) --cgc --ld taglib_more.dll taglib_more.obj user32.lib tag.lib
	cp taglib_more.dll ../../compiled/native/win32/i386
