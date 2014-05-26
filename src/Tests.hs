test1 = do
        let f =  ["a","b","c","d"]
        let c =  [("x",["a","b"]),("y",["c"]),("z",["d"])]
        --let p = runWriter $ prepare_list_of_occurrences f c
        --p1 <- prepare_list_of_occurrences_io (return f) (return c)
        let p2 = prepare_list_of_occurrences_io (return f) (return c)
        --mapM_ (putStrLn) $ snd p

        --let g = group_by_archive_name $ p1

        --g1 <- group_by_archive_name_io $ p2
        let g2 =  group_by_archive_name_io $ p2

        pc ←  prepare_list_of_commands_io g2 command_to_extract
        --mapM_ (putStrLn) $ snd $ pc
        (putStrLn.unlines) pc
 where

{-- ============================================================================================
    ============================================================================================ --}
    command_to_extract ∷  FileName → --file name
                          [FileName] → --archive name
                          String
    command_to_extract a f = "tar -xf \'" ⊕ "bf" ⊕ "/" ⊕ a ⊕ "\' --totals " ++
                                                                           "--verbose " ++ (step1 f)
      where
      step1 ∷  [FileName] → String
      step1 [] = ""
      step1 (a:r) = "\'" ⊕ a ⊕ "\' " ⊕ step1 r
    ------------------------------------------------------------------------------------------------

