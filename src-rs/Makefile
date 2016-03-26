
BUILD=build

expr: $(BUILD)/Expr.rs.bin
	$<

$(BUILD)/%.rs.bin: %.rs | $(BUILD)
	rustc -o $@ $<

$(BUILD):
	mkdir $@

clean:
	rm -rf $(BUILD)
