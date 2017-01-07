# http://teamon.eu/2016/measuring-visualizing-genstage-flow-with-gnuplot/

set terminal png font "Arial,10" size 700,500
set output "logs/progress.png"

set title "Fractal Generation Performance"
set xlabel "Time (ms)"
set ylabel "Items processed"
set key top left # put labels in top-left corner

set xrange [0:15000]

plot "logs/progress-generate_chunk.log" with steps ls 1 title "Generate Chunk", \
     "logs/progress-escape_chunk.log"   with steps ls 2 title "Escape Chunk", \
     "logs/progress-colorize_chunk.log" with steps ls 3 title "Colorize Chunk", \
     "logs/progress-write_chunk.log"    with steps ls 4 title "Write Chunk"
