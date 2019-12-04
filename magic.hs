module Main where
import Data.List

main :: IO ()

main = print(nub [1, 2, 3, 2, 3])

(++++) :: Int -> Int -> Int
x ++++ y = x ^ 2 + y ^ 2


data Position = MakePosition Double Double
distance :: Position -> Position -> Double
distance p1 p2 = 
    case p1 of
        MakePosition x1 y1 -> case p2 of
                                MakePosition x2 y2 -> sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

positionA = MakePosition 0 0
positionB = MakePosition 3 4
distancev = distance positionA positionB


distancePattern :: Position -> Position -> Double
distancePattern (MakePosition x1 y1) (MakePosition x2 y2) =
    sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    
distancevp = distancePattern positionA positionB



{-
listA = [1, 2, 3, 4]
foo [a, b, c, d] = a
foo listA               --1

foo ([a, b] : c) = a    --Not Matching

foo (a : b : c) = c
foo listA               --[3, 4]


foo (a : b : c : d) = d
foo listA               --[4]

foo (a : b : c : d : e) = e
foo listA               --[]


foo (a : b : c : d : e : f) = f --Not Matching
foo (a : b : [c]) = c   --Not Matching

foo (a : b : [c , d]) = c
foo listA               --3

-}


initx :: [a] -> [a]
initx [] = error "..."
initx [x] = []
initx (x : xs) = x : initx xs



takex :: Int -> [a] -> [a]
takex 0 _ = []
takex _ [] = []
takex n (x : xs) = x : takex (n - 1) (x : xs)



--Recursive representation recursive
giveMeFive ::[a] -> [a]
giveMeFive xs = giveMeFiveHelper 0 xs

giveMeFiveHelper :: Int -> [a] -> [a]
giveMeFiveHelper _ [] = []
giveMeFiveHelper i (x : xs) =
    if i `rem` 5 == 0 then x : giveMeFiveHelper (i + 1) xs
                      else giveMeFiveHelper (i + 1) xs
    

    
    
--
intergral :: Double -> [Double] -> [Double]
intergral _ [] = []
intergral acc (x : xs) = let i = x + acc
                            in i : intergral i xs

--intergral 0 [1..10]

--
scanr' :: (b -> a -> a) -> a -> [b] -> [a]
scanr' _ acc [] = [acc]
--scanr' f acc (x : xs) = f x (head ys) : ys
--                        where ys = scanr' f acc xs
scanr' f acc (x : xs) = f x (head (scanr' f acc xs)) : scanr' f acc xs

--scanr' (+) 0 [1, 2, 3]
--[6,5,3,0]
--infixr 5 :



--
scanrx :: (b -> a -> a) -> a -> [b] -> [a]
scanrx _ acc [] = [acc]
scanrx f acc (x : xs) = f x (head (scanrx f acc xs)) : scanrx f acc xs

--scanrx (+) 0 [1, 2]
--                                                 (+) 1 (head (scanrx (+) 0 [2])) : scanrx (+) 0 [2]
--                 (+) 1 (head (scanrx (+) 0 [2]))                                 :      scanrx (+) 0 [2]
--                 (+) 1 (head ((+) 2 (head (scanrx (+) 0 [])) : scanrx (+) 0 [])  :      (+) 2 (head (scanrx (+) 0 [])) : scanrx (+) 0 []
--                 (+) 1 (head ((+) 2 (head [0]) : [0]))                           :      (+) 2 (head [0])               :      [0]
--                 (+) 1 (head ((+) 2 0 : [0]))                                    :      (+) 2 0                        :      [0]
--                 (+) 1 (head (2 : [0]))                                          :      2                              :      [0]
--                 (+) 1 2                                                         :      2                              :      [0]
--                 3                                                               :      2                              :      [0]




--scanrx (+) 0 [1, 2]
--[3,2,0]
--infixr 5 :


--MinSubList
minSubList :: (Num a, Ord a) => [a] -> Int -> a
minSubList xs m = initSum + minDiff
    where
        shifted = drop m xs
        initSum = sum $ take m xs
        minDiff = minimum $ scanl (+) 0 $ zipWith (-) shifted xs

--minSubList [2, 6, 4, 2, 5, 8, 3, 1] 3
--11


minSubListPerformance :: (Num a, Ord a) => [a] -> Int -> a
minSubListPerformance xs m = initSum + minDiff
    where
        (initXs, shifted) = splitAt m xs
        initSum = sum initXs
        minDiff = minimum $ scanl (+) 0 $ zipWith (-) shifted xs


--minSubListPerformance [2, 6, 4, 2, 5, 8, 3, 1] 3
--11




--右折叠实现左折叠
foldlxx :: (a -> b -> a) -> a -> [b] -> a
foldlxx f a bs = foldr (\x y -> f y x) a (reverse bs)

--右折叠实现左折叠
foldlx :: (a -> b -> a) -> a -> [b] -> a
--foldlx f acc bs = foldr (\b g x -> g (f x b)) id bs acc
foldlx f acc bs = foldr (\b g ->(\x -> g (f x b))) id bs acc
--使用闭包变相实现倒序避免reverse开销



--右折叠实现左折叠
--foldlxx :: (a -> b -> a) -> a -> [b] -> a
--foldlxx f a bs = foldr (\x y -> f y x) a (reverse bs)



--首先找个简单的?观察一下看能不能发现一些规律
--foldl plus 0 [1, 2] = plus (plus 0 1) 2
--让我们先试着用各种方法拿foldr来表示一下

--假设存在一个f: Int -> Int使得foldr f 0 [1, 2]可以展开成plus (plus 0 1) 2
--那就会这样

--foldr f 0 [1, 2] = f 1 (f 2 0) = plus (plus 0 1) 2
--这里需要注意我们要求的f并不是简单地让f 1 (f 2 0)的值等于plus (plus 0 1) 2就可以而是需要严格的保证两边的term是一模一样的
--就像coq的reflexivity一样,如果不这样的话对于整数范围上的加法这样满足交换律和结合律的运算来说倒是无所谓,但对不满足交换律或者不满足结合律的运算来说就不行了

--试着求几个f出来可以发现几乎不可能找到一个靠谱的f使得它对任意Int List都有效,这其中最关键的问题在于参数应用的顺序不同
--foldl中我们需要初始值和List中第一个元素先进行运算然后结果和第二个元素进行运算
--但foldr中却是最后一个元素和初始值先运算然后依次向前

--那么有没有办法改变一下参数应用的顺序呢,我的第一反应就是把输入的List做一个reverse,这当然是一个可行的解

--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldl f a bs = foldr (\x y -> f y x) a (reverse bs)
--这样的问题在于需要额外遍历一遍bs比较低效
--那我们能不能用闭包来变相实现倒序功能来避免reverse的开销呢？
--现在的目标是找出一个foldr中使用的g让它在fold过程中把喂给它的元素都偷偷记下来然后在fold结束后倒着把那这些元素进行操作
--如此一来问题就具体成了两个子问题
--如何把元素记下来？
--如何知道fold结束了？
--把元素记在闭包里是已经钦定的,因此g必须可以操作函数,考虑到foldr的类型标记
--foldr :: (b -> a -> a) -> a -> [b] -> a
--不难判断g的第一个参数类型为b第二个参数类型为某个函数类型
--在这个前提下fold结束时得到的是一个函数,此时我们只要给这个函数以参数就可以使它启动然后对闭包中存储的元素倒着来一遍操作
--再结合foldr的类型以及g的类型我们发现此时foldr需要的初始值是一个函数,而foldl给出的初始值却无处可放,因此我们可以把这个初始值作为启动最终操作的信号,一举两得解决了两个麻烦嘛
--接下来开始求解g
--根据前面的分析可以知道g最终生成的函数至少会接受foldl中的初始值,而这个函数的运行结果和这个初始值的类型相同
--为了求解省事我们先尝试这个函数仅有一个参数的情况不行再加,这样一来这个函数的类型也就可以确定了
--让我们来梳理一下目前的已知条件：
--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldr :: (b -> a -> a) -> a -> [b] -> a
--g :: b -> (a -> a) -> (a -> a)
--z' :: a -> a

--foldl f z bs = foldr g z' bs z
--然后带入一个bs试一下

--let bs = [b0, b1]

--foldl f z bs = f (f z b0) b1
--foldr g z' bs z = g b0 (g b1 z') z
--此时在foldl中需要先求值f z b0
--而在foldr中我们惊喜的发现b0和z被拿出来放一起先求值然后尝试解出g

--g :: b -> (a -> a) -> a -> a
--g b0 (g b1 z') z = (g b1 z') (f z b0)
--                 = z' (f (f z b0) b1)
--是不是很眼熟,令z' = id

--let z' = id
--g b0 (g b1 z') z = id (f (f z b0) b1) = f (f z b0) b1
--此时我们解出的g为

--g b f' x = f' (f x b)
--这时我们不妨用其他长度的List来验证一下g的正确性然后你会发现都是没毛病,至于g定义正确性的证明留作思考题,我才不会说是我懒得证了,目测在list长度上归纳是可以证明的

--此时我们的foldl定义就是这样的了
--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldl f z bs = foldr (\b f' x -> f' (f x b)) id bs z
--大功告成(*¯︶¯*)
--总结
--回顾一下解出的答案,我们求得的g实际上就是利用了闭包来形成了一个栈然后通过这个栈来实现运算顺序的颠倒

