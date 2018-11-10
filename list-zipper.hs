data List a = Empty | Cons a (List a)

-- What is a zipper for this?

-- Ans: it's just two lists!
type Zipper a = (List a, List a)

goForward :: Zipper a -> Maybe (Zipper a)
goForward (back, Cons f fs) = Just (Cons f back, fs)
goForward _ = Nothing

goBackward :: Zipper a -> Maybe (Zipper a)
goBackward (Cons b bs, front) = Just (bs, Cons b front)
goBackward _ = Nothing

-- What a cute little data structure.