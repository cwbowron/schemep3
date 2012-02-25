MZC=/cygdrive/c/Program\ Files/PLT/mzc.exe
EXTDIR3M=../../compiled/native/win32/i386/3m
EXTDIRCGC=../../compiled/native/win32/i386

default: system-events-3m.dll
	cp system-events-3m.dll $(EXTDIR3M)/system-events.dll
	
clean:
	rm -f system-events-resources.obj
	rm -f system-events-3m.obj
	rm -f system-events-3m.dll
	rm -f system-events-cgc.obj
	rm -f system-events-cgc.dll
	rm -f $(EXTDIR3M)/system-events.dll
	rm -f $(EXTDIRCGC)/system-events.dll
	
cgc:
	cp system-events-cgc.dll $(EXTDIRCGC)/system-events.dll
	
system-events-resources.obj:	system-events.rc
	windres -o system-events-resources.obj system-events.rc	

system-events-3m.dll: system-events-3m.obj system-events-resources.obj
	$(MZC) --3m --ld system-events-3m.dll system-events-3m.obj user32.lib WtsApi32.lib system-events-resources.obj	

system-events-3m.obj:	system-events.cpp
	cp system-events.cpp system-events-3m.cpp
	$(MZC) --3m --cc system-events-3m.cpp
	rm system-events-3m.cpp

system-events-cgc.dll: system-events-cgc.obj system-events-resources.obj
	mzc --cgc --ld system-events-cgc.dll system-events-cgc.obj user32.lib WtsApi32.lib system-events-resources.obj
	
system-events-cgc.obj:	system-events.cpp
	cp system-events.cpp system-events-cgc.cpp
	$(MZC) --cgc --cc system-events-cgc.cpp
	rm system-events-cgc.cpp
