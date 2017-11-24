> startString = "This old man, he played "
> midString = ", He played knick-knack"
> endString = "; With a knick-knack paddywhack, Give the dog a bone, This old man came rolling home."
> dataString = [("one", "thumb"), ("two", "shoe"), ("three", "knee"), ("four", "door"), ("five", "hive"), ("six", "sticks"), ("seven", "heaven"), ("eight", "gate"), ("nine", "spine"), ("ten", " once again")]

> thisOldMan :: IO ()
> thisOldMan = putStrLn . unlines . map printCouplet $ dataString

> printCouplet :: (String, String) -> String
> printCouplet (n, m)
>   | n == "ten"      = startString ++ n ++ midString ++ m ++ endString
>   | otherwise       = startString ++ n ++ midString ++ " on my " ++ m ++ endString

The original poem was:
This old man, he played one,
He played knick-knack on my thumb; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played two,
He played knick-knack on my shoe; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played three,
He played knick-knack on my knee; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played four,
He played knick-knack on my door; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played five,
He played knick-knack on my hive; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played six,
He played knick-knack on my sticks; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played seven,
He played knick-knack up in heaven; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played eight,
He played knick-knack on my gate; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played nine,
He played knick-knack on my spine; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.

This old man, he played ten,
He played knick-knack once again; With a knick-knack paddywhack, Give the dog a bone,
This old man came rolling home.
