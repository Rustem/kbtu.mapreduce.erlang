SOURCES=map_reduce.erl tag_generator.erl utilities.erl mongotest.erl
OBJECTS=$(SOURCES:.erl=.beam)
ebin:
	mkdir ebin

%.beam: %.erl ebin
	erlc -o ebin $<

mapreduce-project: $(SOURCES) $(OBJECTS)
	cp -R tags ebin

clean:
	rm -rf ebin