# must be on the PATh
RGBASM := rgbasm
RGBLNK := rgblink
RGBFIX := rgbfix

TARGET := $(shell basename $(CURDIR)).gb
SOURCE := $(wildcard *.asm)
OFILES := $(SOURCE:.asm=.obj)

%.obj : %.asm
	$(RGBASM) -o$@ $<

$(TARGET): $(OFILES)
	$(RGBLNK) -o $(TARGET) $(OFILES)
	$(RGBFIX) $(TARGET)

.PHONY: clean
clean:
	rm $(OFILES) $(TARGET)
