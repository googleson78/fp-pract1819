# Полезна `Haskell` информация

## Ресурси

### [Download Haskell], но очевидно за предпочитане е пакетният ви мениджър
Не ви трябва цялата `Haskell Platform`, но е удобна ако искате да ползвате някои по-стандартни библиотеки, които са включени в нея.

### Развлекателна книжка, става и като справочна информация
[Learn You a Haskell for Great Good! (LYAH)]

### Книжка с доста повече "приложения". Има и задачи доста и е много подробна. Надгражда миналата донякъде
[Real World Haskell]

### Як курс който има домашни за почти всяко занятие и материалът е изложен интересно
[Як курс]

### По-"advanced" книга за паралелизъм и конкурентност в езика.
[Паралелно и конкурентно програмиране на Haskell]

### Сайт в който можете да търсите за повече `Haskell`-ски библиотеки
[Stackage]

## IDE?
> `<your-favourite-text-editor>`(`vim`) + `ghci`

## Домашни
### Трябва домашните ви да се компилират с `-Wall` и `-Weror`!!!
Това можете да проверите като ги подадете като аргументи на `ghci`.

Пример:
```
peon :: ~/temp » ghci -Wall -Werror foldl.hs
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/googleson78/.ghci
[1 of 1] Compiling Main             ( foldl.hs, interpreted )

foldl.hs:1:1: error: [-Wmissing-signatures, -Werror=missing-signatures]
    Top-level binding with no type signature:
      (?) :: (a -> b -> c) -> b -> a -> c
  |
1 | (?) = flip
  | ^^^
Failed, no modules loaded.
```

## Style guide
### Предпочитайте функции от по-висок ред пред експлицитна рекурсия!

Подравнявайте си `=`-тата.

Пример:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```

Използвайте `where` и `let` свързвания, за да елиминирате общи изрази, които имат знаечение сами по себе си.

Пример:
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v []     = v
foldl f v (x:xs) = foldl f v' xs
    where
        v' = f v x
```

Използвайте възможно най-много `($)` за да избегнете скоби.

Пример:
```haskell
sumSquaresOfOnlyOdd :: [Integer] -> Integer
sumSquaresOfOnlyOdd xs = foldr (+) 0 $ map (\x -> x * x) $ filter odd xs
```

Предпочитайте [point-free] стил когато можете. (освен ако не трябва да правите магарии за да сработи, очевидно)

Пример:
```haskell
sumSquaresOfOnlyOdd :: [Integer] -> Integer
sumSquaresOfOnlyOdd = foldr (+) 0 . map (\x -> x * x) . filter odd
```

Избягвайте lambda-функции когато можете (освен ако не трябва да правите магарии, за да сработи, очевидно).

Пример:
```haskell
-- instead of
map' :: (a -> b) -> [a] -> [b]
map' = foldr (\x r -> f x : r) []

-- prefer
map'' :: (a -> b) -> [a] -> [b]
map'' = foldr ((:) . f) []
```

[Hoogle]: https://www.haskell.org/hoogle/
[Stackage]: https://www.stackage.org/
[Download Haskell]: https://www.haskell.org/downloads
[Learn You a Haskell for Great Good! (LYAH)]: http://learnyouahaskell.com/chapters
[Real World Haskell]: http://book.realworldhaskell.org/read/
[Як курс]: https://www.seas.upenn.edu/~cis194/spring13/lectures.html
[Паралелно и конкурентно програмиране на Haskell]: https://adjoint.fun/books/haskell/%5BSimon_Marlow%5D_Parallel_and_Concurrent_Programming.pdf
[point-free]: https://wiki.haskell.org/Pointfree
