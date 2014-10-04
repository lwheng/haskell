-- using the Case expression
--
-- case <to be checked> of
-- followed by cases to match
-- "->" is like THEN THIS
-- each "branch" of case must be of the same type

fromMaybe defval wrapped = case wrapped of
                            Nothing -> defval
                            Just value -> value
