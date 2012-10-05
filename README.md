Hello everybody. This is simple Map Reduce application, using Erlang/OTP power of 
concurrency. 
There are two famous problems touched:
    1. word frequences
    2. inverted index - NOT IMPLEMENTED YET

You can test it by using 'project_files' or 'tags' directory. 
To Compile:
  - make mapreduce-project
  - erl
  - cd(ebin).
  - word_counter:count(tags).