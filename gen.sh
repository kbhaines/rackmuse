#!/bin/bash

#racket  midi_inspect.rkt /Users/kevin/Library/CloudStorage/ProtonDrive-marvlogic@protonmail.com-folder/Composing/op42-extn\ -\ Full\ score\ -\ Base.mid --svg-width 1200 --svg-unified --svg-bar-range 109:117 --track-except 16-20 --svg op42-109-117.svg

midi_inspect() {
  INFILE=$1
  OUTFILE=$2
  shift 2
  racket midi-inspect.rkt $INFILE --svg-unified --svg $OUTFILE --svg-width 1200 $*
}

set -e
rktfile=$1
shift 1
for midifile in $*;do
  outfile=$midifile.svg
  racket -l errortrace -t $rktfile
  #midi_inspect $midifile $outfile 
  # midi_inspect $midifile $outfile --svg-overtones 1 --svg-overtones-bloom --track-function
  midi_inspect $midifile $outfile --svg-spectrotone --svg-overtones 1 --svg-overtones-bloom --track-function
  #midi_inspect $midifile $outfile --svg-spectrotone # --svg-bar-range 9:20
  #open -a safari $outfile
done
