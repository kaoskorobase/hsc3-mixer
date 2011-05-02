module Sound.SC3.Mixer.SynthDefs where

import Sound.SC3

mkInsertEffect :: Int -> (UGen -> UGen) -> UGen
mkInsertEffect n f =
    let bus = control KR "bus" 0
        graph = f (in' n AR bus)
        n' = length (mceChannels graph)
    in if n' /= n
       then error $ "mkInsertEffect: Channel mismatch: need " ++ show n ++ " got " ++ show n'
       else replaceOut bus graph

faderDef :: Int -> UGen
faderDef n = mkInsertEffect n $ \i ->
    let level = control KR "level" 1
        mute = control KR "mute" 0
    in i * level * (1 - mute)

patchCord :: UGen
patchCord =
    let inbus = control KR "in" 0
        outbus = control KR "out" 0
    in out outbus (in' 1 AR inbus)
