

x=\x->foldl (-) (head x) (drop 1 x)
int32 x=x


\x->foldl (-) ((extI32 . head) x ) (((map extI32) . (drop 1)) x)