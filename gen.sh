#!/bin/bash

#racket  midi_inspect.rkt /Users/kevin/Library/CloudStorage/ProtonDrive-marvlogic@protonmail.com-folder/Composing/op42-extn\ -\ Full\ score\ -\ Base.mid --svg-width 1200 --svg-unified --svg-bar-range 109:117 --track-except 16-20 --svg op42-109-117.svg

set -e
rktfile=$1
shift 1
for midifile in $*;do
  outfile=$midifile.svg
  racket -l errortrace -t $rktfile
#racket midi-inspect.rkt $midifile --svg-width 1200  --svg-unified  --svg-overtones 1 --svg-overtones-bloom --svg $outfile
  racket midi-inspect.rkt $midifile --svg-width 1200  --svg-unified  --svg-spectrotone --svg $outfile
# racket midi-inspect.rkt $midifile --svg-width 1200  --svg $outfile
#racket midi-inspect.rkt $midifile --svg-width 1200  --svg-unified  --svg $outfile
#open -a safari $outfile
done
