SOURCES=map_reduce.erl word_counter.erl inverted_index.erl utilities.erl
OBJECTS=$(SOURCES:.erl=.beam)
ebin:
	mkdir ebin

%.beam: %.erl ebin
	erlc -o ebin $<

mapreduce-project: $(SOURCES) $(OBJECTS)
	cp -R tags ebin

clean:
	rm -rf ebin