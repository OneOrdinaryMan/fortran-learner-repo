CC=gfortran
CCFLAGS=-Wall -Wextra -c -o
LINKER=gfortran
LINKERFLAGS=-Wall -Wextra -o
TARGET=arithmetic_and_logical_operators
TARGET_DIR=target
TARGET_FILE=$(TARGET_DIR)/$(TARGET)
SRC_DIR=src
MAIN_FILE=$(SRC_DIR)/main.f90
MAIN=$(TARGET_DIR)/main.o
.DEFAULT_GOAL:=build
.PHONY: clean remake
build: $(TARGET_FILE) 
$(TARGET_FILE): $(MAIN)
	$(LINKER) $(LINKERFLAGS) $@ $<
$(MAIN): $(MAIN_FILE) | $(TARGET_DIR)
	$(CC) $(CCFLAGS) $@ $<
$(TARGET_DIR):
	-mkdir $@
clean:
	@{\
		if [ -d $(TARGET_DIR) ];then\
			rm -r $(TARGET_DIR);\
		fi;\
	}
remake: clean build
run: $(TARGET_FILE)
	./$<
