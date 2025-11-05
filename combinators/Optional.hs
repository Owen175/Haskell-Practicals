import Combinators

-- identity - i x = x
i = S:@K:@K

-- combinator - w x = x x
w = S:@i:@i

--omega = K:@