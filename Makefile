# vim: noet
#
SRC_DIR := src
BUILD_DIR := build

all: $(BUILD_DIR)
	cd $(BUILD_DIR) && ghc -o sa -threaded -hidir . -odir . ../$(SRC_DIR)/ConnectDB.hs ../$(SRC_DIR)/Types.hs ../$(SRC_DIR)/BeerShopDB.hs ../$(SRC_DIR)/Main.hs

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

run: all
	./$(BUILD_DIR)/sa

.PHONY: all clean run
