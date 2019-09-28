Monada State

1. Тип State

newtype State s a = State { runState::(s -> (a, s)) }
:k State * -> * -> *

st <- State  		runState st :: s -> (a, s)

evalState st s = fst (runState st s)
execState st s = snd (runState st s)

get :: State s s
get = State (\s -> (s, s))
put :: s -> State s ()
put s = State (\_ -> ((), s)) 

2. Monada State

instance Monad (State s) where
 return :: a -> State s a
 return a = State (\s -> (a, s))

 f :: a -> State s b

 (State x) >> = f = State (\s -> let (a, s1) = x s              let (a, s1) = x s
								     (State q) = f a			in runState (f a) s1
								 in q s1)

3. Control.Monad.State

StateT 		tyoe State s = StateT s Identity
newtype Identity a = Identity { runIdentity::a }

stInt :: State Int String
stInt = do num <- get
           put 59
           return ("Take " ++ show num)

runState stInt 49 = ( "Take 49" , 59 )

вхід S | вихід (a, s)
   49  |  (49, 49) 
   49  |  ((), 59)
   59  |  ("Take 49", 59)

4. System.Random

r.n = (a * r.n-1 + b) mod c 

class RandomGen g where
	genRange :: g -> (Int, Int)
	next :: g -> (Int, g)
	split :: g -> (g, g)

class RandomGen g where
	data StatGen = ....    read(show g) = g
	mkStdGen :: Int -> StdGen
	getStdGen :: IO StdGen

class Random a where
	randomR :: RandomGen g => (a, a) -> g -> (a, g)
	random :: RandomGen g => g (a, g)
	randomRs :: RandomGen g => (a, a) -> g -> [a]
	randomS :: ..... => g -> [a]

	oneBool :: IO Bool
	oneBool = do g <- getGtdGen
			     let (b, g1) = random g  // (b, g1) <- return (random g)
			     print(b::Bool)

data MyType = MT Int Bool Char Int deriving Show

fmRandom :: StdGen -> (MyType, StdGen)
fmRandom g = let (n, g1) = randomR (0, 100) g
			     (b, g2) = randomR g1
			     (c, g3) = randomR ('a', 'z') g2
			     (m, g4) = randomR (-n, n) g4
			 in (MT n b c m, g4)

getAny :: (Random a) => State StdGen a
getAny = do g <- get
            let (x, g1) = random g
            put g1
            return x

вхід S | вихід (s, s)     //на вході генератор
   g   |  (g, g) 
   g   |  ((), g1)
   g1  |  (x, g1)