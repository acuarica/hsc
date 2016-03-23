
BUILD=build

sup: $(BUILD)/Supercompiler.hs.bin
	$<

app: $(BUILD)/Append.hs.bin
	$<

expr: $(BUILD)/Expr.rs.bin
	$<

word: $(BUILD)/WordCount.hs.bin
	$<

$(BUILD)/%.hs.bin: %.hs | $(BUILD)
	ghc -odir $(BUILD) -o $@ $<

$(BUILD)/%.rs.bin: %.rs | $(BUILD)
	rustc -o $@ $<

$(BUILD):
	mkdir $@

clean:
	rm -rf $(BUILD)
