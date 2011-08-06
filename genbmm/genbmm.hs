import System

bmm :: String -> String
bmm c = unlines
  [ "ADDRESS_SPACE code RAMB18 [0x00000000:0x000074FF]"
  , ""
  , "BUS_BLOCK"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[0].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [17:0];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[1].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [35:18];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[2].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [53:36];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[3].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [71:54];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[4].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [89:72];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[5].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [107:90];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[6].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [125:108];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[7].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [143:126];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[8].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [161:144];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[9].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [179:162];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[10].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [197:180];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[11].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [215:198];"
  , "red_instance/" ++ c ++ "/BU2/U0/blk_mem_generator/valid.cstr/" ++
    "ramloop[12].ram.r/v5_init.ram/SP.SINGLE_PRIM18.SP [233:216];"
  , ""
  , "END_BUS_BLOCK;"
  , ""
  , "END_ADDRESS_SPACE;"
  ]

main =
  do args <- getArgs
     case args of
       [i] -> putStrLn (bmm i)
       _ -> error "Usage: genbmm.hs INSTANCE"
