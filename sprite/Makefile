# must be on the PATh
RGBASM := rgbasm
RGBLNK := rgblink
RGBFIX := rgbfix

TARGET := $(shell basename $(CURDIR)).gb
SOURCE := $(wildcard *.agb)
OFILES := $(SOURCE:.agb=.obj)

%.obj : %.agb
	$(RGBASM) -o$@ $<

$(TARGET): $(OFILES)
	$(RGBLNK) -o $(TARGET) $(OFILES)
	$(RGBFIX) $(TARGET)

.PHONY: clean
clean:
	rm $(OFILES) $(TARGET)
