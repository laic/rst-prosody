Some scripts used for the quotation detection experiments.

## Some example calls needed to deal with textgrids is in proc-conv.sh

* Feature extraction wrapper: ./proc-conv.sh
- this uses prosodic features extraction code I wrote from another project. 
You can find a close enough version in my github repo:
	https://github.com/laic/prosody

* Collated word features: look at the main() function in collate-word-feats.r 
- this R file contains various helper functions used in the classification
experiments.

* Experiments: (Sorry this is a bit of a mess!)
	## Turn level experiments
        * turn-results.r:  no RS turn segments vs RS segments
        * turn.nors-results.r:  Whether a turn contains any RS or not
        * turn.i-results.r: I vs no-RS 
        * rpros-results.r: D v I prosody

        ## Boundary (word level) experiments:
        * re-results.r: RS end detection, only look at turns known to contain RS
        * re-all-results.r: RS end detection, look at all turns 
        * rs-results.r: RS start detection, only look at turns known to contain RS
        * rs-all-results.r: RS start detection, look at all turns 

