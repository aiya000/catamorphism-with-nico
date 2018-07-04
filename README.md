# example codes for 矢澤にこ先輩と一緒にcatamorphism！

```
      Maybe Char -----> Char
       |                 |
fmap f |                 | f
       v                 v
      Maybe Int  -----> Int

    (Fix Maybe, Maybe (Fix Maybe) -> Fix Maybe)
         !                             !
         !                             !
         v                             v
(Char, Maybe Char -> Char) --> (Int, Maybe Int -> Int)
```

# :diamond_shape_with_a_dot_inside: ナニコレ？ :diamond_shape_with_a_dot_inside:
　にこちゃんがcatamorphismやF-代数について解説するSS「矢澤にこ先輩と一緒にcatamorphism！」に掲載したコードの全体 :dog2:
