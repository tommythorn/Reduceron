Red15 and Red16 represent the final implementations of the Reduceron.
Red16 is intended to be a cleaned-up, easier to read version of Red15.
However, only recently I discovered that I have introduced a bug in
moving from Red15 to Red16 that spoils the reducer after the first
garbage collection.  I didn't have time to fix Red16, so Red15 is
the final, working version of Reduceron.
